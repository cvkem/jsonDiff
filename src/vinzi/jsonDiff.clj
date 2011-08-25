(ns vinzi.jsonDiff
  (:require  [clojure
	      [zip :as zip]])
  (:use [clojure.pprint])
  (:use [vinzi.jsonZip])
  )

;; definition of a patch
;; and fully qualified labels to be used for the actions
(defrecord Patch [pathList action key value])
(def actDelete :delete)
(def actInsert :insert)
(def actChange :change)




;; helper routines


(defn- testZipperRootPos [zipper msg]
  (if (and (vector? zipper)
	   (= 2 (count zipper)))
  (when (not= nil (zip/up zipper))
    (println "WARNING: " msg " is not located at root position"))
  (do
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
  ([org mod] 
     (testZipperRootPos org  "findPatches/org")
     (testZipperRootPos mod "findPatches/mod")
     (assert (= (jsonKey org) (jsonKey mod)))
     (findPatchesZipper org mod [] []))
  ([org mod patches path]
     (let [parPath path
	   path (if (nil? org)
		  path
		  (conj path (jsonKey org)))
	   orgNode (if (nil? org) nil (zip/node org))
	   modNode (if (nil? mod) nil (zip/node mod))
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
	   ;; extend the patchList
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
				  patches (findPatchesZipper org mod p pl) ]
			      patches))
	   getPatches (fn [so sm p pl pef]
			;; get the patches for this node. 'pef' is a higher order funtion
			;; used to process the values for basic kvs and compounds (children)."
			   ;; (println "\nEnter getPatchesKvs: ")
			   ;; (print " with so: ") (pprint so)
			   ;; (print " with sm: ") (pprint sm)
			   ;; (print " with p: ") (pprint p)
			   ;; (println)
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
	   getSortChildren (fn [zipper]
			     (loop [child   (zip/down zipper)
				    children [] ]
			       (if (and (not (nil? child))
					(not (nil? (zip/node child))))
				 (let [jKey (jsonKey child)
				       childNode (zip/node child)
				       element [jKey childNode child] ]
				   (recur (zip/right child) (conj children element)))
				 ;; vectors are sorted by the index value, maps on the key value
				 (sort-by first children))))
	   getSortVector (fn [zipper]
			     (loop [child   (zip/down zipper)
				    children [] ]
			       (if (and (not (nil? child))
					(not (nil? (zip/node child))))
				 (let [jKey (jsonKey child)
				       childNode (zip/node child)
				       element [jKey childNode child] ]
				   (recur (zip/right child) (conj children element)))
				 ;; vectors are sorted by the index value, maps on the key value
				 (sort-by first children))))
	   ;; insertRestVect (fn [pat mod]
	   ;; 		    (if (nil? mod)
	   ;; 		      pat
	   ;; 		      (let [node (zip/node mod)
	   ;; 			    _   (println " EXPECTING ERROR")
	   ;; 			    rec  [(jsonKey mod) node] ]   
	   ;; 			  (recur (insertPatch rec patches parPath) (zip/right node))))
	   ;; 		    )
	   ]
       (if (and (nil? orgNode) (nil? modNode))  ;; both nil, so we are ready
	 patches
	 (if (and (not (nil? orgNode)) (nil? modNode)) ;; (mod nil, so delete next value of org)
	   ;; (let [nPatches  (deletePatch [(jsonKey org)] patches path)
	   ;; 	 nOrg     (zip/right org)
	   ;; 	 _        (do (println " nOrg" nOrg)
	   ;; 		      (println " mod " mod)
	   ;; 		      (println "length of new patchlist = " (count nPatches))
	   ;; 		      (flush))]
	   ;; 				;(findPatchesZipper (zip/right org) mod nPatches  parPath)
	     patches   ;; delete of patches happens in previous recursion  ??
	   (if (nil? orgNode)   ;; org is nil (and mod is not nil) so insert a value
	     (let [rec  [(jsonKey mod) modNode] ]
	       (findPatchesZipper org (zip/right mod) (insertPatch rec patches parPath) parPath))
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
		     (let [sortOrgChild (getSortChildren org)
			   sortModChild (getSortChildren mod)
			   ;; _  (do (println "sortModChild heeft " (count sortModChild) " elements.")
			   ;; 	(pprint sortModChild) (flush))
			   patches (getPatches sortOrgChild sortModChild patches path childPatchEquals)]
		       ;; (println "The patches are: ")
		       ;; (pprint patches)
		       patches))
		   ;;  else Both of type vector
		   (let [sortOrgChild (getSortVector org)
			 sortModChild (getSortVector mod)]
		     (getPatches sortOrgChild sortModChild patches path childPatchEquals)))
		 ;; both compound nodes are of a different type (apply change);
		 ;; Thus: insert a change patch and return
		 (let [rec  [(jsonKey mod) modNode] ]  ;; could be a deletion OR a change (how to choose??)
		   (changePatch rec patches parPath)))
	       ;; else-part: at least one of the nodes is not of a compound type
	       (if (or (map? orgNode)  
		       (map? modNode))
		 ;; One node of compound type, other of basic type.
		 ;; assume deletion (of a vector element) from the original (here we only support appends to vector)
		 (findPatchesZipper (zip/right org) mod (deletePatch [(jsonKey orgNode)] patches path) parPath)
		 (if (= (str orgNode) (str modNode))  
		   patches  ;; both basic types are equal, so we're ready on this path
		   (let [
			 ;; _   (do
			 ;;       (print "orgNode = ")
			 ;;       (pprint orgNode)
			 ;;       (print "modNode = ")
			 ;;       (pprint modNode)
			 ;;       (flush)
			 ;;       (pprint (jsonKey mod))
			 ;;       (flush))
			 rec  [(jsonKey mod) modNode] ]  ;; could be a deletion OR a change (assume no inserts on vector)
		     (findPatchesZipper (zip/right org) (zip/right mod) (changePatch rec patches parPath) parPath)))))))))))

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

