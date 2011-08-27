(ns vinzi.jsonDiff
  (:require  [clojure
	      [zip :as zip]])
  (:use [clojure.pprint])
  (:use [vinzi.jsonZip])
  )

;;;; temporay include debugger

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))


(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input ::tl)
      exit-code
      input)))


(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))


(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))


(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=" x#) (flush)) x#))
(defmacro dbg1 [x] `(let [x# ~x] (do (println '~x "=" x#) (flush)) x#))
(defmacro dbgp [x] `(let [x# ~x] (do (println '~x "=") (pprint x#) (flush)) x#))
(defmacro dbgm [m x] `(let [x# ~x] (do (println ~m " : " '~x "=" x#) (flush)) x#))
(defmacro dbgm1 [m x] `(let [x# ~x] (do (println ~m " : " '~x "=" x#) (flush)) x#))

(defmacro dbg [x] x)
(defmacro dbg1 [x] x)
(defmacro dbgp [x] x)
(defmacro dbgm [m x] x)
(defmacro dbgm1 [m x] x)

;;;; end debugger


;; definition of a patch
;; and fully qualified labels to be used for the actions
(defrecord Patch [pathList action key value])
(def actDelete :delete)
(def actInsert :insert)
(def actChange :change)




;; helper routines


(defn- testZipperRootPos [zipper msg]
  (if (and (not= nil zipper)
	   (isZipper? zipper))
  (when (not= nil (zip/up zipper))
    (println "WARNING: " msg " is not located at root position"))
  (do   ;; zipper is not a jsonZipper!!
    (println "\n") 
    (println "ERROR (" msg "): is not a (json-)Zipper")
    (pprint zipper)
    (flush)
    (let [z (jsonZipper zipper)]
      (println "tried to change to zipper")
      (pprint z))
    (flush)
    (assert false)
    (println "THIS SHOULD NEVER BE EXECUTED") (flush))))


(defn subZip
  "Creates a subtree of the current node of zipper. Used exclude the current left- and right neigbors from the tree."
  [zipper]
  (jsonZipper (zip/node zipper)))

;; core code

(defn findPatchesZipper
  "Search the set of patches needed to translate zipper 'org' to zipper 'mod'. In the case
   that elements are vectors it is assumed that additions are only made at tail-position
   of the vector. The routine assumes ordering of elements is hashmaps is irrelevant."
  ([org mod] 
     (testZipperRootPos org  "findPatches/org")
     (testZipperRootPos mod "findPatches/mod")
     (assert (= (jsonKey org) (jsonKey mod)))
     (findPatchesZipper org mod [] [] false))
  ([org mod patches path quitOnPatch]
     (let [parPath path
	   path (if (nil? org)
		  path
		  (conj path (jsonKey org)))
	   orgNode (if (nil? org) nil (zip/node org))
	   modNode (if (nil? mod) nil (zip/node mod))
	   parIsVect (fn [z] (and (dbg1 z)
				  (when-let [par (zip/up z)]
				    (= (jsonType par) jsonTypeVector))))
	   ;; if the direct parent is a vector than it should perform nog right-recur
	   ;; (is handled by getVectorPatches itself)
	   rightRecur (not (or (dbg1 (parIsVect org)) (dbg1 (parIsVect mod))))  
	   getSortKvs  (fn [data]
			 (let [kvs (map vector (keys data) (vals data))
			       kvs (filter #(not= (first %) :jsonChildren) kvs)  ;; exclude the :jsonChildren
			;;       _  (do (println "getSortKvs: for data " data)
			;;	      (apply pprint kvs)
			;;	      (flush))
			      ]
			   (if (seq kvs)
			     (sort-by first kvs)
			     ())))
	   ;;
	   ;; three functions used to extend a patchList 'p' with change 'rec' and location/pathlist 'pl'
	   deletePatch    (fn [rec p pl]
			    (conj p (Patch. pl actDelete (first rec) nil)))
	   insertPatch    (fn [rec p pl]
			    (let [k      (first rec)
				  valZip (second rec)
				  valJson (zippertreeToJson valZip)]
			;;      (print "Trying to insert tree: ") (pprint valZip)
			;;      (print "....  as json: ") (pprint valJson)
			;;      (flush)
			      (conj p (Patch. pl actInsert k valJson))))
	   changePatch    (fn [rec p pl] (conj p (Patch. pl actChange (first rec) (zippertreeToJson (second rec)))))
	   kvsPatchEquals (fn [fso fsm p pl]
			    (let [vo (second fso)
				  vm (second fsm)
				  ;; iterate over the second field of fso and fsm (the actual node (not the zipper)
				  patches (if (= vo vm)
					    p                                ;; same value
					    (changePatch fsm p pl))]  ;; different value
			      patches))
	   childPatchEquals (fn [fso fsm p pl]
			    (let [org (nth fso 2)
				  mod (nth fsm 2)
				  ;; iterate over the org and mod zippers (third field of fso and fsm)
				  patches (findPatchesZipper org mod p pl quitOnPatch) ]
			      patches))
	   getChildren (fn [zipper]
			     (loop [child   (zip/down zipper)
				    children [] ]
			       (if (and (not (nil? child))
					(not (nil? (zip/node child))))
				 (let [jKey (jsonKey child)
				       childNode (zip/node child)
				       element [jKey childNode child] ]
				   (recur (zip/right child) (conj children element)))
			     children)))
	   getSortChildren (fn [zipper]
			     (sort-by first (dbg (getChildren zipper))))
	   ;;
	   ;;  two sub-functions that locate patches with parameters:
	   ;;     - so :   list of children of origin
	   ;;     - mo :   list of chilrden for modified
	   ;;     -  p :   list of (previous) patches
	   ;;     - pl :   pathlist of current location
	   ;;     - pef:   higher order function to make getPatches more generic.
	   getPatches (fn [so sm p pl pef]
			;; This getPatches is used when children are sorted on a (stable) key.
			;; get the patches for this node. 'pef' is a higher order funtion
			;; used to process the values for basic kvs and compounds (children).
			;;
			;; check special case 'so empty'
			   (if (not (seq so))
			     (if (not (seq sm))
			       p   ;; both so and sm are empty (return the patches)
			       (recur nil (next sm) (insertPatch (first sm) p pl) pl pef))
			     ;; check special case 'sm empty' (and so not empty)
			     (if (not (seq sm))
			       (let [nPatches (deletePatch (first so) p pl)]
				 (recur (next so) nil nPatches pl pef))
			       ;; both so and sm are non-empty
			       (let [
;;				     _  (do (println  "so: " (ffirst so) " sm: " (ffirst sm))
;;					    (println " full (first sm) "  (first sm) )
;;					    (flush)) 
				     keyCompare (compare (name (ffirst so)) (name (ffirst sm))) ]
				 (if (= 0 keyCompare)
				   ;; same key, so compare values
				   (let [patches (pef (first so) (first sm) p pl)]
				     (recur (next so) (next sm) patches pl pef))
				   ;; different keys, so progress the right one
				   (if (> keyCompare 0)   ;; so > sm
				     (recur so (next sm) (insertPatch (first sm) p pl) pl pef)
				     (let [nPatches (deletePatch (first so) p pl)]
				       (recur (next so) sm nPatches pl pef))))))))
	   mergePatches    (fn [p nPatches] (vec (concat p nPatches)))
	   renumNewPatches (fn [p nPatches pl numDel]
			     ;; returns a new patchlist that combines p and nPatches
			     ;; where the keys and paths of nPatches are numbered down by numDel 
			     (let [posPath (count pl)
				   ;; repair a single key
				   translateKey (fn [key]
						  (when key
						    (println "original key = " key)
						    (str "[" (- (Integer/parseInt
								 (subs key 1 (dec (count key)))) numDel) "]")))
				   ;; repair a single patch
				   repairPatch (fn [patch]
						 (let [path (:pathList patch)
						       pKey (translateKey (get path posPath))
						       nPath (if (nil? pKey)
							      path
							      (assoc path posPath pKey))
						       nKey (if (nil? pKey)
							      (translateKey (:key patch))
							      (:key patch))]
						   (assoc patch
						     :pathList nPath
						     :key   nKey)))]
			     (mergePatches p (map repairPatch nPatches))))
	   getVectorPatches (fn getVectorPatches [so sm p pl]
			      ;; getVectorPatches is used as keys in vectors are not stable as insertion
			      ;; and deletion of elements will result in a shift of keys.
			      ;; currently this function only supports detection of change, deletion and appends.
			      ;;  (inserts are not supported)
			      ;; check special case 'so empty'
			      (if (not (seq so))
				(if (not (seq sm))
				  p   ;; both so and sm are empty (return the patches)
				  (recur nil (next sm) (insertPatch (first sm) p pl) pl))
				;; check special case 'sm empty' (and so not empty)
				(if (not (seq sm))
				  (let [nPatches (deletePatch (first so) p pl)]
				    (recur (next so) nil nPatches pl))
				  ;; 
				  ;; both so and sm are non-empty
				  (let [o (first so)
					m (first sm)
					oz (nth o 2)
					mz (nth m 2)
					newPatches (dbg1 (findPatchesZipper (dbg1 oz) (dbg1 mz) [] pl quitOnPatch))]
				    (if (empty? newPatches)
				      ;; fn-recur (no additional patches)
				      (getVectorPatches  (rest so) (rest sm) p pl)
				      ;; else: there are patches (accept patches or delete a serie of vector-elements? 
				      (loop [no (next so)
					     nm (next sm)
					     numDel 1]
					(if (empty? no)
					  ;; no more candidates, so fn-recur without renumbering
					  (getVectorPatches (rest so) (rest sm)
							    (mergePatches p newPatches) pl)
					  ;; we have a new candidate for matching current 'mz'
					  (let [cand  (nth (first no) 2)
						candPatches (findPatchesZipper cand mz [] pl true)]
					    (if (empty? candPatches)
					      ;; 'cand' matches 'mz' exactly, introduce the sequence of deletePatches
					      (let [delPatches (last (take (inc numDel)
									   (iterate #(deletePatch o % pl) [])))
						    ;; and continue at succesor 'no' in org
						    newPatches (getVectorPatches (rest no) (rest sm) [] pl)]
						(print "patches where: ") (pprint p)
						(println "numDel : " numDel)
						(print "delPatches are: ") (pprint delPatches)
						(print "further patches before renum: ") (pprint newPatches)
						(flush)
						(dbg1 (mergePatches p (renumNewPatches delPatches newPatches pl numDel))))
					      ;; no match on current 'mz'. Test whether candidate matches on 'nm'
					      (let [nmz (if (empty? nm) nil (nth (first nm) 2))]
						(if (or (nil? nmz)
							(seq (findPatchesZipper cand nmz [] pl true)))
						  ;; a loop-recur (no exact match of cand and counterpart 'nmz')
						  (recur (next no) (next nm) (inc numDel))
						  ;; else part, cand matches on counterpart in mod.
						  ;; So accept patches and perform a fn-recur
						  (do
						    (print "newPatches --> ") (pprint newPatches)
						    (print " p --> ") (pprint p) (flush)
						    (getVectorPatches (rest so) (rest sm)
							   (mergePatches p newPatches) pl)))))))))))))

	   ;; getSortVector (fn [zipper]
	   ;; 		     (loop [child   (zip/down zipper)
	   ;; 			    children [] ]
	   ;; 		       (if (and (not (nil? child))
	   ;; 				(not (nil? (zip/node child))))
	   ;; 			 (let [jKey (jsonKey child)
	   ;; 			       childNode (zip/node child)
	   ;; 			       element [jKey childNode child] ]
	   ;; 			   (recur (zip/right child) (conj children element)))
	   ;; 			 ;; vectors are sorted by the index value, maps on the key value
	   ;; 			 (sort-by first children))))
	   ;; insertRestVect (fn [pat mod]
	   ;; 		    (if (nil? mod)
	   ;; 		      pat
	   ;; 		      (let [node (zip/node mod)
	   ;; 			    _   (println " EXPECTING ERROR")
	   ;; 			    rec  [(jsonKey mod) node] ]   
	   ;; 			  (recur (insertPatch rec patches parPath) (zip/right node))))
	   ;; 		    )
	   ]
       (dbgp orgNode)
       (dbgp modNode)
       (dbgp patches)
       
       (flush)
       (if (or (and (nil? orgNode) (nil? modNode))  ;; both nil, so this branch is finished
	       (and false quitOnPatch (seq patches)))    ;; quit if there are differences!
	 (dbgm1 "EXIT-1" patches)     ;; RETURN
	 (if (and (not (nil? orgNode)) (nil? modNode)) ;; (mod nil, so delete next value of org)
	   ;; (let [nPatches  (deletePatch [(jsonKey org)] patches path)
	   ;; 	 nOrg     (zip/right org)
	   ;; 	 _        (do (println " nOrg" nOrg)
	   ;; 		      (println " mod " mod)
	   ;; 		      (println "length of new patchlist = " (count nPatches))
	   ;; 		      (flush))]
	   ;; 				;(findPatchesZipper (zip/right org) mod nPatches  parPath)
	     (dbgm1 "EXIT-2" patches)   ;; RETURN: delete of patches always happens in previous recursion  ??
	   (if (nil? orgNode)   ;; org is nil (and mod is not nil) so insert a value
	     (let [rec  [(jsonKey mod) modNode] ]
	       (dbgm1 "EXIT-3" (let [newPatches (insertPatch rec patches parPath)]
				 (if rightRecur
				   (findPatchesZipper org (zip/right mod) newPatches parPath quitOnPatch)
		      newPatches))))
	     (if (and (map? orgNode)  ;; Both are compound values (map or vector!!)
		      (map? modNode))
	       ;; both nodes are of a compound type (check whether same type)
	       (if (= (jsonType org) (jsonType mod)) 
		 ;; 
		 ;; Both nodes are of same type (both vector or both map;
		 (if (= (jsonType org) jsonTypeMap)
		   ;; Both of type hashmap
		   ;; Thus: Process the basic keys   (will be empty list for a vector)   ...
		   (let [sortOrg (getSortKvs orgNode)
			 sortMod (getSortKvs modNode)
			 patches (getPatches sortOrg sortMod patches path kvsPatchEquals)]
		     ;; ... and next process the compound keys  (stored in ':jsonChildren')
		     (let [sortOrgChild (dbg (getSortChildren org))
			   sortModChild (dbg (getSortChildren mod))
			   ;; _  (do (println "sortModChild heeft " (count sortModChild) " elements.")
			   ;; 	(pprint sortModChild) (flush))
			   patches (getPatches sortOrgChild sortModChild patches path childPatchEquals)]
		       ;; (println "The patches are: ")
		       ;; (pprint patches)
		       (dbgm1 "EXIT-4" patches)))
		   ;;  else 'orgNode' and 'modNode' are of type vector
		   (let [_   (or (assert (= (jsonType org) jsonTypeVector))
				 (assert (= (count orgNode) 1))
				 (assert (= (count modNode) 1)))
			 orgChild (getChildren org)
			 modChild (getChildren mod)
			 _   (println)
			 _   (println "\n\nENTER getVectorPatches")
			 newPatches (getVectorPatches orgChild modChild [] path)]
		     (dbgm1 "EXIT-5" (mergePatches patches newPatches))))  ;; merge the patches and RETURN
		 ;; both compound nodes are of a different type (apply change);
		 ;; Thus: insert a change patch and return
		 (let [rec  [(jsonKey mod) modNode] ]  
		   (changePatch rec patches parPath)))  ;; RETURN
	       ;; else-part: at least one of the nodes is not of a compound type
	       (if (or (map? orgNode)  
		       (map? modNode))
		 ;; One node of compound type, other of basic type.
		 ;; assume deletion (of a vector element) from the original (here we only support appends to vector)
		 (dbgm1 "EXIT-6" (let [newPatches (deletePatch [(jsonKey org)] patches path)]
				   (if rightRecur
				     (findPatchesZipper (zip/right org) mod newPatches parPath quitOnPatch)
				     newPatches)))
		 (if (= (str orgNode) (str modNode))  
		   (dbgm1 "EXIT-7" patches)  ;; both basic types are equal. RETURN
		   (let [;; NOTE: check whether we can still end up here for a vector!!
			 ;; could be a deletion OR a change (assume no inserts on vector)
			 rec  [(jsonKey mod) modNode]
 			 newPatches (changePatch rec patches parPath)]
		     (if rightRecur
		       (dbgm1 "EXIT-8" (findPatchesZipper (zip/right org) (zip/right mod)
							  newPatches parPath quitOnPatch))
			      newPatches )))))))))))

(defn findPatchesJson
  "Find the patches to translate json-object 'org' to 'mod'. Wrapper that calls 'findPatchesZipper'
  (See 'findPatchesZipper' for details)."
  [org mod]
  (let [orgZip (jsonZipper org)
	modZip (jsonZipper mod)]
    (findPatchesZipper orgZip modZip)))

(defn applyPatchesZipper
  "Applies the 'patches' to jsonZipper 'org' and returns the modified zipper"
  [org patches]
  )

(defn applyPatchesJson
  "Applies the 'patches' to json-object 'org' and returns the modified object.
   Wrapper that calls 'applyPatchesZipper' (See 'applyPatchesZipper' for details)."
  [org patches]
  (zip/root (applyPatchesZipper (jsonZipper org) patches)))

;;;;; temp test

(def o3data {:x {:a 1
		   :b "test-string"}})

(def o3 (jsonZipper o3data))


;; test 3a
(def m3a (jsonZipper
	    {:x {:a 1
	        :a1 {:x 1}
	        :b "test-string"}}))

(def p3a [
	     (Patch. ["/" :x] actInsert :a1 {:x 1} )])



(def p4a [
	     (Patch. ["/" "[1]"] actInsert :a1 {:x 1} )])
;;(findPatchesZipper o3 m3a)

(def o5n (jsonZipper [1
			 2
			 3
			 4
			 5]))

(def m5n (jsonZipper [1 "changed" 5]))

(def p5n [(Patch. ["/"] actChange "[1]" "changed")
	      (Patch. ["/"] actDelete "[2]"  nil)
	      (Patch. ["/"] actDelete "[2]" nil)])

(def ms5n "delete fields at position 2-4 and modified position 3")

(findPatchesZipper o5n m5n)