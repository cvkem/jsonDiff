(ns vinzi.jsonDiff
;;  (:use [clojure.walk :only [postwalk]])
  (:require  [clojure
	      [zip :as zip]])
  ;; 	      [string :as s]])
  ;; (:require  [clojure.contrib
  ;; 	      [sql :as sql]
  ;; 	      [json :as json]
  ;; 	      [duck-streams :as ds :only [reader]]])
  (:use [clojure.pprint])
;;  (:use [alex-and-georges.debug-repl :as dr])
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
	   path (conj path (jsonKey org))
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
	   deletePatch    (fn [rec p pl] (conj p (Patch. pl actDelete (first rec) nil)))
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
				  patches (if (= vo vm)
					    p                                ;; same value
					    (changePatch fsm p pl))]  ;; different value
			      patches))
	   childPatchEquals (fn [fso fsm p pl]
			    (let [org (nth fso 2)
				  mod (nth fsm 2)
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
			       (recur (next so) nil (deletePatch (first so) p pl) pl pef)
			       ;; both so and sm are non-empty
			       (let [
				     ;;_  (do (println  "so: " (ffirst so) " sm: " (ffirst sm)) (flush)) 
;;				     keyCompare (compare (ffirst so) (ffirst sm)) ]
				     keyCompare (compare (name (ffirst so)) (name (ffirst sm))) ]
				     
				 (if (= 0 keyCompare)
				   ;; same key, so compare values
				   (let [patches (pef (first so) (first sm) p pl)]
				   ;; TO DO (move to higher order function
				   ;; (let [vo (second (first so))
				   ;; 	 vm (second (first sm))
				   ;; 	 patches (if (= vo vm)
				   ;; 		   p                                ;; same value
				   ;; 		   (changePatch (first sm) p pl))]  ;; different value
				     ;; end higher order that updated the patches
				     (recur (next so) (next sm) patches pl pef))
				   ;; different keys, so progress the right one
				   (if (> keyCompare 0)   ;; so > sm
				     (recur so (next sm) (insertPatch (first sm) p pl) pl pef)
				     (recur (next so) sm (deletePatch (first so) p pl) pl pef)))))))
	   getSortChildren (fn [zipper]
			     (loop [child   (zip/down zipper)
				    children [] ]
			       (if (not (nil? child))
				 (let [jKey (jsonKey child)
				       childNode (zip/node child)
				       element [jKey childNode child] ] 
				   (recur (zip/right child) (conj children element))) 
				 (sort-by first children))))
	   insertRestVect (fn [pat mod]
			    (if (nil? mod)
			      pat
			      (let [node (zip/node mod)
				    rec  [(jsonKey node) node] ]
				  (recur (insertPatch rec patches parPath) (zip/right node))))
			    )]
       (if (and (nil? orgNode) (nil? modNode))  ;; both nil, so we are ready
	 patches   
	 (if (and (not (nil? orgNode)) (nil? modNode)) ;; (mod nil, so delete next value of org)
	   (findPatchesZipper (zip/right org) mod  (deletePatch nil patches path) parPath)
	   (if (nil? orgNode)   ;; org is nil (and mod is not nil) so insert a value
	     (let [rec  [(jsonKey modNode) modNode] ]
	       (findPatchesZipper org (zip/right mod) (insertPatch rec patches parPath) parPath))
	     (if (and (map? orgNode)  ;; Both are compound values (map or vector!!)
		      (map? modNode))
	       ;; both items are of a compound type
	       ;; process the basic keys   (will be empty list for a vector)
	       (let [sortOrg (getSortKvs orgNode)
		     sortMod (getSortKvs modNode)
		     ;; _   (println "printing sorted org and mod kvs")
		     ;; _   (pprint sortOrg)
		     ;; _   (pprint sortMod)
		     patches (getPatches sortOrg sortMod patches path kvsPatchEquals)]
		 ;; and process the compound keys  (the ':jsonChildren')
		 (let [sortOrgChild (getSortChildren org)
		       sortModChild (getSortChildren mod)
		       ;; _  (do
		       ;; 	    (println "sorted org children") (pprint sortOrgChild)
		       ;; 	    (println "sorted mod children") (pprint sortModChild)
		       ;; 	    )
		       patches (getPatches sortOrgChild sortModChild patches path childPatchEquals)]
		   (println "The patches are: ")
		   (pprint patches)
		   patches))
	       (if (or (map? orgNode)  
		       (map? modNode))
		 ;; One node of compound type, other of basic type.
		 ;; assume deletion (of a vector element) from the original (here we only support appends to vector)
		 (findPatchesZipper (zip/right org) mod (deletePatch nil patches path) parPath)
		 (if (= (str orgNode) (str modNode))  
		   patches  ;; both basic types are equal, so we're ready on this path
		   (let [rec  [(jsonKey modNode) modNode] ]  ;; could be a deletion OR a change (how to choose??)
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
  "Applies the 'patches' to jsonZipper 'org'. Wrapper that calls 'applyPatchesZipper'
  (See 'applyPatchesZipper' for details)."
  [org patches]
  (zip/root (applyPatchesZipper (jsonZipper org) patches)))

