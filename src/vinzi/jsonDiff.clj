(ns vinzi.jsonDiff
  (:require  [clojure
              [string :as str]
	      [zip :as zip]])
  (:use [clojure.pprint])
;;  (:use [clojure.walk])    ;; for debugging only
  (:use [vinzi.jsonZip])
  )


(defn- patchErr
  "Use the zipErrors queue such that errors of both libraries are properly ordered (interleaved). The error is added to the zipErrs message-queue. The boolean 'printZipErrors' determines whether the error will be printed to *out* too."
  ([msg] (patchErr msg nil))
  ([msg ret] (zipError (str "PATCH-ERROR:" msg))))




(defmacro if2
  "This macro provides an if-statement that enforces the usage of an else-clause. (In a normal else statement a small error in the bracketing can cause that the else-part becomes part of the surrounding code (always gets executed). "
  [c t f]
  `(if ~c ~t ~f))


(comment ;; debugger
  (defmacro diff_dm [m x] `(let [x# ~x]
			     (do (println ~m " : " '~x "=" x#) (flush)) x#))
  (defmacro diff_dbg [x] x)
)  ;;;; end debugger
(defmacro diff_dbg [x] x)
(defmacro diff_dm [m x] x)


;; definition of a patch
;; and fully qualified labels to be used for the actions
(defrecord Patch [pathList action key value])
(def actDelete :delete)
(def actInsert :insert)
(def actChange :change)



;;;;;;;;;
;; debugging/print routines

(comment   ;; for debugging
  
(defn- strCompare [x y]
  (compare (str x) (str y)))

(defn- sort-map [m]
  (into (sorted-map-by strCompare) m))

(defn- sort-set [s]
  (into (sorted-set-by strCompare) s))

(defn- sForm [f]
  (if (map? f)
    (sort-map f)
    (if (set? f)
      (sort-set f)
      f)))

(defn- sortForm1 [f]
  (postwalk sForm f))

(defn- equalForms1 [x y]
  (let [sx  (sortForm1 x)
	sy  (sortForm1 y)
	res (strCompare sx sy)]
    (when (not= res 0)
      (println  "DIFFERENCE:   " res)
      (print "with org: ") (pprint sx)
      (print "with mod: ") (pprint sy))
    (= res 0)))

(defn- equalFormsPrint [x y]
  (let [sx  (sortForm1 x)
	sy  (sortForm1 y)
	res (strCompare sx sy)]
    (if (not= res 0)
      (do
	(println  "DIFFERENCE:   " res)
	(print "with org: ") (pprint sx)
	(print "with mod: ") (pprint sy))
      (do
	(println "form have length " (count (str sx))  "  and " (count (str sy)))
	(println "first form::::")
	(pprint sx)))
    (= res 0)))


(defn- printNode [msg zip]
  (print "NODE: " msg " has ")
  (if zip
    (let [node (zip/node zip)
	  nc (count (:jsonChildren node))
	  bNode (dissoc node :jsonChildren)]
      (println nc " children and contents")
      (println bNode)))
  (println "zipper = nil"))



(defn- basicCompare [org mod]
  (let [prepNode (fn [z]
		   (let [n (zip/node z)
			 c (count (:jsonChildren n))]
		     (assoc (dissoc n :jsonChildren) :childCount c)))
	o (prepNode org)
	m (prepNode mod)]
   (equalForms1 o m)))

)  ;; end comment  (for debugging)

;;;;;;;;;;;;
;; helper routines
;;

(defn- testZipperRootPos [zipper msg]
  (if2 (and (not= nil zipper)
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



;; core code

(defn findPatchesZipper
  "Search the set of patches needed to translate zipper 'org' to zipper 'mod'. In the case
   that elements are vectors it is assumed that additions are only made at tail-position
   of the vector. The routine assumes ordering of elements is hashmaps is irrelevant."
  ([org mod] (findPatchesZipper org mod false))
  ([org mod quitOnPatch]
     (testZipperRootPos org  "findPatches/org")
     (testZipperRootPos mod "findPatches/mod")
     (assert (= (jsonKey org) (jsonKey mod)))
     (findPatchesZipper org mod [] [] quitOnPatch))
  ([org mod patches path quitOnPatch]
     (let [parPath path
	   path (if2 (nil? org)
		  path
		  (conj path (jsonKey org)))
	   orgNode (if2 (nil? org) nil (zip/node org))
	   modNode (if2 (nil? mod) nil (zip/node mod))
	   parIsVect (fn [z] (and (diff_dbg z)
				  (when-let [par (zip/up z)]
				    (= (jsonType par) jsonTypeVector))))
	   ;; if the direct parent is a vector than it should perform nog right-recur
	   ;; (is handled by getVectorPatches itself)
	   rightRecur (not (or (diff_dbg (parIsVect org)) (diff_dbg (parIsVect mod))))  
	   getSortKvs  (fn [data]
			 (let [kvs (map vector (keys data) (vals data))
			       kvs (filter #(not= (first %) :jsonChildren) kvs)  ;; exclude the :jsonChildren
			      ]
			   (if2 (seq kvs)
			     (sort-by first kvs)
			     ())))
	   ;;
	   ;; three functions used to extend a patchList 'p' with change 'rec' and location/pathlist 'pl'
	   deletePatch    (fn [rec p pl]
			    (conj (diff_dm "delete-PATCH added to" p) (Patch. pl actDelete (first rec) nil)))
	   insertPatch    (fn [rec p pl]
			    (let [k      (first rec)
				  valZip (second rec)
				  valJson (zippertreeToJson valZip)]
			      (conj (diff_dm "insert-PATCH added to" p) (Patch. pl actInsert k valJson))))
	   changePatch    (fn [rec p pl] (conj (diff_dm "change-PATCH added to" p) (Patch. pl actChange (first rec) (zippertreeToJson (second rec)))))
	   kvsPatchEquals (fn [fso fsm p pl]
			    (let [vo (second fso)
				  vm (second fsm)
				  ;; iterate over the second field of fso and fsm (the actual node (not the zipper)
				  
				  patches (if2 (diff_dm (str "comparing " vo "  and " vm) (= vo vm))
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
			       (if2 (and (not (nil? child))
					(not (nil? (zip/node child))))
				 (let [jKey (jsonKey child)
				       childNode (zip/node child)
				       element [jKey childNode child] ]
				   (recur (zip/right child) (conj children element)))
			     children)))
	   getSortChildren (fn [zipper]
			     (sort-by first (diff_dbg(getChildren zipper))))
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
			   (if2 (not (seq so))
			     (if2 (not (seq sm))
			       (diff_dm "result of getPatches" p)   ;; both so and sm are empty (return the patches)
			       (recur nil (next sm) (insertPatch (first sm) p pl) pl pef))
			     ;; check special case 'sm empty' (and so not empty)
			     (if2 (not (seq sm))
			       (let [nPatches (deletePatch (first so) p pl)]
				 (recur (next so) nil nPatches pl pef))
			       ;; both so and sm are non-empty
			       (let [
				     keyCompare (compare (name (ffirst so)) (name (ffirst sm))) ]
				 (if2 (diff_dm (str (name (ffirst so)) "<->" (name (ffirst sm))) (= 0 keyCompare))
				   ;; same key, so compare values
				   (let [patches (pef (first so) (first sm) (diff_dm (str "#=" (count p)) p) pl)]
				     (recur (next so) (next sm) patches pl pef))
				   ;; different keys, so progress the right one
				   (if2 (> keyCompare 0)   ;; so > sm
				     (recur so (next sm) (insertPatch (first sm) p pl) pl pef)
				     (let [nPatches (deletePatch (first so) p pl)]
				       (recur (next so) sm nPatches pl pef))))))))
	   getPatchesChildren (fn [org mod p pl]
				    (let [sortOrgChild (diff_dbg(getSortChildren org))
					  sortModChild (diff_dbg(getSortChildren mod))]
				      (getPatches sortOrgChild sortModChild p path childPatchEquals)))
	   mergePatches    (fn [p nPatches] (vec (concat p nPatches)))
	   renumNewPatches (fn [p nPatches pl numDel]
			     ;; returns a new patchlist that combines p and nPatches
			     ;; where the keys and paths of nPatches are numbered down by numDel 
			     (let [posPath (count pl)
				   ;; repair a single key
				   translateKey (fn [key]
						  (when key
						    (str "[" (- (Integer/parseInt
								 (subs key 1 (dec (count key)))) numDel) "]")))
				   ;; repair a single patch
				   repairPatch (fn [patch]
						 (let [path (:pathList patch)
						       pKey (translateKey (get path posPath))
						       nPath (if2 (nil? pKey)
							      path
							      (assoc path posPath pKey))
						       nKey (if2 (nil? pKey)
							      (translateKey (:key patch))
							      (:key patch))]
						   (assoc patch
						     :pathList nPath
						     :key   nKey)))]
			     (mergePatches p (map repairPatch nPatches))))
	   getVectPat (fn getVectPat [so sm p pl]
			      ;; getVectPat is used as keys in vectors are not stable as insertion
			      ;; and deletion of elements will result in a shift of keys.
			      ;; currently this function only supports detection of change, deletion and appends.
			      ;;  (inserts are not supported)
			      ;; check special case 'so empty'
			(if2 (not (seq so))
				(if2 (not (seq sm))
				  (diff_dm "result of getVectPat" p)   ;; both so and sm are empty (return the patches)
				  (recur nil (next sm) (insertPatch (first sm) p pl) pl))
				;; check special case 'sm empty' (and so not empty)
				(if2 (not (seq sm))
				  (let [nPatches (deletePatch (first so) p pl)]
				    (recur (next so) nil nPatches pl))
				  ;; 
				  ;; both so and sm are non-empty
				  (let [o (first so)
					m (first sm)
					oz (nth o 2)
					mz (nth m 2)
					newPatches (diff_dm "gvp" (findPatchesZipper (diff_dbg oz) (diff_dbg mz) [] pl quitOnPatch))]
				    (if2 (empty? newPatches)
				      ;; fn-recur (no additional patches)
				      (getVectPat  (rest so) (rest sm) p pl)
				      ;; else: there are patches (accept patches or delete a serie of vector-elements? 
				      (do
					(loop [no (next so)
					     nm (next sm)
					     numDel 1]
					(if2 (empty? no)
					  ;; no more candidates, so fn-recur without renumbering
					  (getVectPat (rest so) (rest sm)
							    (mergePatches p newPatches) pl)
					  ;; we have a new candidate for matching current 'mz'
					  (let [cand  (nth (first no) 2)
						candPatches (findPatchesZipper cand mz [] pl true)]
					    (if2 (empty? candPatches)
					      ;; 'cand' matches 'mz' exactly, introduce the sequence of deletePatches
					      (let [delPatches (last (take (inc numDel)
									   (iterate #(deletePatch o % pl) [])))
						    ;; and continue at succesor 'no' in org
						    newPatches (getVectPat (rest no) (rest sm) [] pl)]
						(diff_dbg (mergePatches p (renumNewPatches delPatches newPatches pl numDel))))
					      ;; no match on current 'mz'. Test whether candidate matches on 'nm'
					      (let [nmz (if2 (empty? nm) nil (nth (first nm) 2))]
						(if2 (or (nil? nmz)
							(seq (findPatchesZipper cand nmz [] pl true)))
						  ;; a loop-recur (no exact match of cand and counterpart 'nmz')
						  (recur (next no) (next nm) (inc numDel))
						  ;; else part, cand matches on counterpart in mod.
						  ;; So accept patches and perform a fn-recur
						    (getVectPat (rest so) (rest sm)
								(mergePatches p newPatches) pl)))))))
					))))))
	   getVectorPatches (fn [org mod p pl]
			      ;; determine whether vector is indexed by index or by Vector-id
			      (let [orgId  (getVectId org)
				    modId  (getVectId mod)]
				(if2 (not= orgId modId)
				  (patchErr (format (str "The origing uses vector-id %s "
							    "and the modified version uses %s.\n"
							    "Can not process this substree !!" orgId modId)))
				  (if2 orgId
				    (getPatchesChildren org mod p pl)
				    (let [_   (or (assert (= (jsonType org) jsonTypeVector))
						  (assert (= (count orgNode) 1))
						  (assert (= (count modNode) 1)))
					  orgChild (getChildren org)
					  modChild (getChildren mod)]
				      (diff_dm  "ENTRY to getVectPat" (getVectPat orgChild modChild p pl)))))))
	   ]
;;       (diff_dm "NEW findPatchZipper" path)
;;       (basicCompare org mod)
					;       (printNode "org" org)
;       (printNode "mod" mod)
       ;; (if2 org (diff_dm (str "name=" (:name orgNode) " ") (jsonKey org)) (println "org=nil")) 
       ;; (if2 mod (diff_dm (str "name=" (:name modNode) " ") (jsonKey mod)) (println "mod=nil")) 
       ;; (flush)
       (if2 (or (and (nil? orgNode) (nil? modNode))  ;; both nil, so this branch is finished
	       (and false quitOnPatch (seq patches)))    ;; quit if there are differences!
	 (diff_dm "EXIT-1" patches)     ;; RETURN
	 (if2 (and (not (nil? orgNode)) (nil? modNode)) ;; (mod nil, so delete next value of org)
	   ;; (let [nPatches  (deletePatch [(jsonKey org)] patches path)
	   ;; 	 nOrg     (zip/right org)
	   ;; 	 _        (do (println " nOrg" nOrg)
	   ;; 		      (println " mod " mod)
	   ;; 		      (println "length of new patchlist = " (count nPatches))
	   ;; 		      (flush))]
	   ;; 				;(findPatchesZipper (zip/right org) mod nPatches  parPath)
	     (diff_dm "EXIT-2" patches)   ;; RETURN: delete of patches always happens in previous recursion  ??
	   (if2 (nil? orgNode)   ;; org is nil (and mod is not nil) so insert a value
	     (let [rec  [(jsonKey mod) modNode] ]
	       (diff_dm "EXIT-3" (let [newPatches (insertPatch rec patches parPath)]
				 (if2 rightRecur
				   (findPatchesZipper org (zip/right mod) newPatches parPath quitOnPatch)
		      newPatches))))
	     (if2 (and (map? orgNode)  ;; Both are compound values (map or vector!!)
		      (map? modNode))
	       ;; both nodes are of a compound type (check whether same type)
	       (if2 (= (jsonType org) (jsonType mod)) 
		 ;; 
		 ;; Both nodes are of same type (both vector or both map;
		 (if2 (= (jsonType org) jsonTypeMap)
		   ;; Both of type hashmap
		   ;; Thus: Process the basic keys   (will be empty list for a vector)   ...
		   (let [sortOrg (getSortKvs orgNode)
			 sortMod (getSortKvs modNode)
			 patches (diff_dm "patches of kvs" (getPatches sortOrg sortMod patches path kvsPatchEquals))]
		     ;; ... and next process the compound keys  (stored in ':jsonChildren')
		     (diff_dm "EXIT-4" (diff_dm " + patches children" (getPatchesChildren org mod patches path))))
		   ;;  else 'orgNode' and 'modNode' are of type vector
		   (diff_dm "EXIT-5" (getVectorPatches org mod patches path)))
		 ;; both compound nodes are of a different type (apply change);
		 ;; Thus: insert a change patch and return
		 (let [rec  [(jsonKey mod) modNode] ]  
		   (changePatch rec patches parPath)))  ;; RETURN
	       ;; else-part: at least one of the nodes is not of a compound type
	       (if2 (or (map? orgNode)  
		       (map? modNode))
		 ;; One node of compound type, other of basic type.
		 ;; assume deletion (of a vector element) from the original (here we only support appends to vector)
		 (diff_dm "EXIT-6" (let [newPatches (deletePatch [(jsonKey org)] patches path)]
				   (if2 rightRecur
				     (findPatchesZipper (zip/right org) mod newPatches parPath quitOnPatch)
				     newPatches)))
		 (if2 (= (str orgNode) (str modNode))  
		   (diff_dm "EXIT-7" patches)  ;; both basic types are equal. RETURN
		   (let [;; NOTE: check whether we can still end up here for a vector!!
			 ;; could be a deletion OR a change (assume no inserts on vector)
			 rec  [(jsonKey mod) modNode]
 			 newPatches (changePatch rec patches parPath)]
		     (if2 rightRecur
		       (diff_dm "EXIT-8" (findPatchesZipper (zip/right org) (zip/right mod)
							  newPatches parPath quitOnPatch))
			      newPatches )))))))))))

(defn findPatchesJson
  "Find the patches to translate json-object 'org' to 'mod'. Wrapper that calls 'findPatchesZipper'
  (See 'findPatchesZipper' for details)."
  [org mod]
  (let [orgZip (jsonZipper org)
	modZip (jsonZipper mod)]
    (findPatchesZipper orgZip modZip)))

(defn jsonChanged?
  "Checks whether there are (material) differences between the two objects (at least one patch difference). Quits on detecting the first difference/patch."
  [org mod]
  (let [orgZip (jsonZipper org)
	modZip (jsonZipper mod)]
    (findPatchesZipper orgZip modZip true)))



(defn applyPatchesZipperAux
  "Applies the 'patches' to jsonZipper 'org' and returns the modified zipper. Zipper will be a the location of the last modification (or it's predecessor in case of a delete operations.)."
  [org patches]
  (let [applyDelete (fn [loc pathList key value]
		      (assert (not value))
		      (removeItem loc pathList key))
	applyInsert (fn [loc pathList key value]
		      (insertItem loc pathList key value))
	applyChange (fn [loc pathList key value]
		      (replaceItem loc pathList key value))
	actMap {actDelete applyDelete
		actInsert applyInsert
		actChange applyChange}]
    (if2 (seq patches)
	 (let [cPatch (diff_dm "Processing patch " (first patches))
	       {:keys [pathList action key value]} cPatch 
	    actionFunc (action actMap)
	    mod (actionFunc org pathList key value)]
	(if mod
	  (recur mod (rest patches))
	  (do
	    (patchErr (format "Apply failed on patch %s (patch ignored)" cPatch))
	    (recur org (rest patches)))))
      org)))


(defn applyPatchesZipper
  "Applies the 'patches' to jsonZipper 'org' and returns the modified zipper at a root location."
  [org patches]
  (let [res (zipTop (applyPatchesZipperAux org patches))]
    res))
  

(defn applyPatchesJson
  "Applies the 'patches' to json-object 'org' and returns the modified object.
   Wrapper that calls 'applyPatchesZipper' (See 'applyPatchesZipper' for details)."
  [org patches]
  (if-let [modified (applyPatchesZipper (jsonZipper org) patches)]
    (jsonRoot modified)
    nil))

;;;;;;;;;;;;;;;;;;;;;;
;;  Translators to store path-list as string and retrieve it from a string.


(defn keywordize
  "keywordize the string 'x' if it starts with a colon"
  [x]
  (if (= (first x) \:) (keyword (apply str (rest x))) x))

(defn getPathList
  "Path is a  string (generated by getPathStrPatch) that needs to be translated back to a pathList."
  [path]
    (if (or (= path "/") (= path ""))
      (vector path)  ;; treat as special case
      (let [path (str/split path #"/")
	    path (map keywordize path)
	    path (if (= (first path) "")
		   (cons "/" (rest path)) path)]
	(vec path))))

(defn getPathStrPatch
  "Get a string representation of the path list of the patch."
  [patch]
 {:pre [(= (str (type patch)) "class vinzi.jsonDiff.Patch")]} 
 (let [pList  (:pathList patch)
	head   (first pList)]
    (if (and (= head "") (<= (count pList) 1))
      (if (= (:key patch) "/")  "" "/")
      (if (= head "/")
	(apply str "/" (interpose "/" (rest pList)))
	(println "ERROR in format pathList: " pList)))))

;; (defn getPathStrPatch
;;   "Get a string representation of the path list of the patch."
;;   [patch]
;;  {:pre [(= (str (type patch)) "class vinzi.jsonDiff.Patch")]} 
;;   (let [pList  (:pathList patch)
;; 	head   (first pList)]
;;     (if (and (= head "") (<= (count pList) 1)) ""
;; 	(if (= head "/")
;; 	  (apply str (interpose "/" (rest pList)))
;; 	  (println "ERROR in format pathList: " pList)))))


(comment   ;; test code to read a large datafile (and check for patches)
(use 'clojure.java.io)
(use 'clojure.data.json)

(def s (with-open [f (reader "../cdfdeMgt/data/EIS-poc.cdfde")] (read-json f)))
(def s1 (with-open [f (reader "../cdfdeMgt/data/EIS-mod.cdfde")] (read-json f)))

(println "comparing forms s and s1")
(equalFormsPrint s s1)

(println "Finding patches for s and s1")
(doseq [p (findPatchesJson s s1)] (println p))

)



(comment  ;; test code

;; temporary patch

(def org6efg (jsonZipper [-10
			 -11
			 -12
			 -13
			 -14]))

(def mod6e (jsonZipper [1 5]))

(def patch6e [(Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)])

(def patch6a [(Patch. ["/"] actDelete "[1]" nil)])
(def patch6c [(Patch. ["/"] actDelete "[2]" nil)])

(def patch6b [(Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)])


(println " One deleted") 
(pprintJsonZipper (applyPatchesZipper org6efg patch6e))

(println "\n\n two deleted")
(pprintJsonZipper (applyPatchesZipper org6efg patch6a))

(println "\n\n three deleted")
(pprintJsonZipper (applyPatchesZipper org6efg patch6b))

(println "\n\n position [2] deleted")
(pprintJsonZipper (applyPatchesZipper org6efg patch6c))

);;; test code 
