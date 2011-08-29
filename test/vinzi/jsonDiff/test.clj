(ns vinzi.jsonDiff.test
  (:use [vinzi.jsonDiff] :reload)
  (:import [vinzi.jsonDiff Patch])
;;  (:require  [clojure.contrib
;;	      [json :as json]
;;	      [duck-streams :as ds :only [writer]]
	      ;;	      [sql :as sql]
;;	      ])
  (:use [vinzi.jsonZip :only [jsonZipper jsonRoot jsonKey]])
  (:use [clojure.test])
  (:use [clojure.walk])
  (:use [clojure.pprint])
  (:require  [clojure  [zip :as zip]])
  )


(defn- strComp [x y]
  ;; (print "Comparing x = ")
  ;; (pprint x)
  ;; (print "     with y = ")
  ;; (pprint y)
  (compare (str x) (str y)))

(defn- sort-map [m]
  (into (sorted-map-by strComp) m))

(defn- sort-set [s]
  (into (sorted-set-by strComp) s))

(defn- sForm [f]
  (if (map? f)
    (sort-map f)
    (if (set? f)
      (sort-set f)
      f)))

(defn- sortForm [f]
  (postwalk sForm f))


(defn- canonicalZip [z]
" Extract the json-object and bring it in a canonical form"
  (str (sForm (jsonRoot z))))

(defn compareZip [x y]
  (let [sx  (canonicalZip x)
	sy  (canonicalZip y)
	res (strComp sx sy)]
    (when (not= res 0)
      (println  "equalForms:   " res)
      (print "with x: ") (pprint sx)
      (print "with y: ") (pprint sy))
    (= res 0)))
;;  (compare (canonicalZip x) (canonicalZip y)))

;;;;;;;;;;;;
;; macro's for comparison
;;

(defmacro discoverTest [patch org mod msg]
  `(is (strComp ~patch (findPatchesZipper ~org ~mod))  (str " DISCOVER-TEST:" ~msg)))

(defmacro applyTest [patch org mod msg]
  `(is (compareZip  (applyPatchesZipper ~org ~patch) ~mod)  (str " APPLY-TEST:" ~msg)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; test cases 1:  changing of basic key (at top level)
;;

(def org1 (jsonZipper {:a 1
	   :b "test-string"}))

;; test 1a
(def mod1a (jsonZipper {:a 1
	   :b "mod-string"}))

(def patch1a [
	     (vinzi.jsonDiff.Patch. ["/"] actChange :b "mod-string")])

(def msg1a "change of string at top level")
     
;; test 1b
(def mod1b (jsonZipper {:a "1"
	   :b "test-string"}))

(def patch1b [
	     (Patch.  ["/"] actChange :a "1")])

(def msg1b "change of int to String at top level")
     
;; test 1c
(def mod1c (jsonZipper {:a 1}))

(def patch1c [
	      (Patch. ["/"] actDelete :b nil)])

(def msg1c "delete last field of hashmap")

;; test 1d
(def mod1d (jsonZipper {:a 1
			:b "test-string"
			:c "inserted :c"}))

(def patch1d [
	     (Patch. ["/"] actInsert :c "inserted :c")])

(def msg1d " added field at tail position")

;; test 1e
(def mod1e (jsonZipper {:a 1
			:b "test-string"
			:aa "inserted :a1"}))

(def patch1e [
	     (Patch. ["/"] actInsert :aa "inserted :a1")])

(def msg1e " added field at tail position")

;; test 1f
(def mod1fdata [ 1
			"test-string"
			"inserted :a1"])
(def mod1f (jsonZipper mod1fdata))

(def patch1f [
	     (Patch. [] actChange "/"  mod1fdata)])

(def msg1f " changed type from map to vector at root position")


;; test 1g
(def mod1g (jsonZipper {:b "test-string"}))

(def patch1g [
	     (Patch. ["/"] actDelete :a nil)])

(def msg1g " delete field at first position of map")

;; test 1h
(def mod1h (jsonZipper {:a 1}))

(def patch1h [
	     (Patch. ["/"] actDelete :b nil)])

(def msg1h " delete field at last position of map")


(deftest test1 ;; 
  (discoverTest patch1a org1 mod1a  msg1a)
  (discoverTest patch1b org1 mod1b  msg1b)
  (discoverTest patch1c org1 mod1c  msg1c)
  (discoverTest patch1d org1 mod1d  msg1d)
  (discoverTest patch1e org1 mod1e  msg1e)
  (discoverTest patch1f org1 mod1f  msg1f)
  (discoverTest patch1g org1 mod1g  msg1g)
  (discoverTest patch1h org1 mod1h  msg1h)
  )

(deftest test1reverse   ;; apply patches and see whether you get the original
  (applyTest patch1a org1 mod1a  msg1a)
  (applyTest patch1b org1 mod1b  msg1b)
  (applyTest patch1c org1 mod1c  msg1c)
  (applyTest patch1d org1 mod1d  msg1d)
  (applyTest patch1e org1 mod1e  msg1e)
  (applyTest patch1f org1 mod1f  msg1f)
  (applyTest patch1g org1 mod1g  msg1g)
  (applyTest patch1h org1 mod1h  msg1h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test cases 2:  changing of compound key (at top level)
;;

(def org2data {:a 1
	       :b "test-string"})

(def org2 (jsonZipper org2data))


;; test 2a
(def mod2a (jsonZipper
	    {:a 1
	     :a1 {:x 1}
	     :b "test-string"}))

(def patch2a [
	     (Patch. ["/"] actInsert :a1 {:x 1} )])

(def msg2a " add a compound key at top level")

;; test 2b
(def mod2b (jsonZipper
	    {:a 1
	     :cc {:x 1}
	     :b "test-string"}))

(def patch2b [
	     (Patch. ["/"] actInsert :cc {:x 1} )])

(def msg2b " add a compound key at top level tail position")

;; test 2c
(def mod2cdata  {:a 1
		 :cc [1 2 3]
		 :b "test-string"})
(def mod2c (jsonZipper mod2cdata))


(def patch2c [
	      (Patch. ["/"] actInsert :cc [1 2 3] )])

(def msg2c " add a compound key at top level tail position")


(deftest test2 ;; 
  (discoverTest patch2a org2 mod2a  msg2a)
  (discoverTest patch2b org2 mod2b  msg2b)
  (discoverTest patch2c org2 mod2c  msg2c)
  )

(deftest test2reverse ;; 
  (applyTest patch2a org2 mod2a  msg2a)
  (applyTest patch2b org2 mod2b  msg2b)
  (applyTest patch2c org2 mod2c  msg2c)
  )

(println "test2reverse temporarily excluded")


;;;;;;;;;;;;;;;;;;;;;;
;; test cases 3:   adding keys at a level deeper in map
;;

(def org3data {:x {:a 1
		   :b "test-string"}})

(def org3 (jsonZipper org3data))


;; test 3a
(def mod3a (jsonZipper
	    {:x {:a 1
	        :a1 {:x 1}
	        :b "test-string"}}))

(def patch3a [
	     (Patch. ["/" :x] actInsert :a1 {:x 1} )])

(def msg3a " add a compound key at second level within map")

;; test 3b
(def mod3b (jsonZipper
	    {:x {:a 1
		 :cc {:x 1}
		 :b "test-string"}}))

(def patch3b [
	     (Patch. ["/" :x] actInsert :cc {:x 1} )])

(def msg3b " add a compound key at second level of map at tail position")

;; test 3c
(def mod3cdata  {:x {:a 1
		     :cc [1 2 3]
		     :b "test-string"}})
(def mod3c (jsonZipper mod3cdata))


(def patch3c [
	      (Patch. ["/" :x] actInsert :cc [1 2 3] )])

(def msg3c " add a compound key at second level tail position")


(deftest test3 ;; 
  (discoverTest patch3a org3 mod3a  msg3a)
  (discoverTest patch3b org3 mod3b  msg3b)
  (discoverTest patch3c org3 mod3c  msg3c)
  )

(deftest test3reverse ;; 
  (discoverTest patch3a org3 mod3a  msg3a)
  (discoverTest patch3b org3 mod3b  msg3b)
  (discoverTest patch3c org3 mod3c  msg3c)
  )

;;;;;;;;;;;;;;;;
;; test cases 4:   adding keys at second level deeper where top-level is a vector)
;;

(def org4data [0
	       {:a 1
		:b "test-string"}
               2])

(def org4 (jsonZipper org4data))


;; test 4a
(def mod4a (jsonZipper
	    [0
	     {:a 1
	      :a1 {:x 1}
	      :b "test-string"}
	     2]))

(def patch4a [
	     (Patch. ["/" "[1]"] actInsert :a1 {:x 1} )])

(def msg4a " add a compound key at second level within vector")

;; test 4b
(def mod4b (jsonZipper
	    [ 0
	      {:a 1
		 :cc {:x 1}
	       :b "test-string"}
	     2]))

(def patch4b [
	     (Patch. ["/" "[1]"] actInsert :cc {:x 1} )])

(def msg4b " add a compound key at second level of map at tail position")

;; test 4c
(def mod4cdata  [ 0
		 {:a 1
		     :cc [1 2 3]
		     :b "test-string"}
		  2 ])
(def mod4c (jsonZipper mod4cdata))


(def patch4c [
	      (Patch. ["/" "[1]"] actInsert :cc [1 2 3] )])

(def msg4c " add a compound key at second level tail position of map")

;; test 4d
(def org4d (jsonZipper [{:a 1
			 :b "test-string"}
			1
			2]))

(def mod4d (jsonZipper [ {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 1
			 2 ]))


(def patch4d [
	      (Patch. ["/" "[0]"] actInsert :cc [1 2 3] )])

(def msg4d " add a compound key in a map that is the first element of a vector")

;; test 4e
(def org4e (jsonZipper [{:a 1
			 :b "test-string"}
			1
			2]))

(def mod4e (jsonZipper [ {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 1
			 2 ]))


(def patch4e [
	      (Patch. ["/" "[0]"] actInsert :cc [1 2 3] )])

(def msg4e " add a compound key at second level at last position of a vector")


;; test 4f

(def org4f (jsonZipper [{:a 1
			 :b "test-string"}
			:one
			2]))

(def mod4f (jsonZipper [ {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 :one
			 2 ]))


(def patch4f [
	      (Patch. ["/" "[0]"] actInsert :cc [1 2 3] )])

(def msg4f " add a compound key at position (vector contains both  numbers and keywords)")




(deftest test4 ;; 
  (discoverTest patch4a org4 mod4a  msg4a)
  (discoverTest patch4b org4 mod4b  msg4b)
  (discoverTest patch4c org4 mod4c  msg4c)
  (discoverTest patch4d org4d mod4d  msg4d)
  (discoverTest patch4e org4e mod4e  msg4e)
  (discoverTest patch4f org4f mod4f  msg4f)
  )

(deftest test4reverse ;; 
  (applyTest patch4a org4 mod4a  msg4a)
  (applyTest patch4b org4 mod4b  msg4b)
  (applyTest patch4c org4 mod4c  msg4c)
  (applyTest patch4d org4d mod4d  msg4d)
  (applyTest patch4e org4e mod4e  msg4e)
  (applyTest patch4f org4f mod4f  msg4f)
  )


;;;;;;;;;;;;;
;; test 5: all tests are looking for an empty set of patches (no ghost-pathces)
;;

;; test 5a
(def om5a (jsonZipper [1
			2]))
(def patch5empty [])
(def msg5a " compare vector to unchanged vector")


(def om5b (jsonZipper [ {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 :one
			 2 ]))
(def msg5b " compare unmodified vec-map-vec (map head position)")

(def om5c (jsonZipper [ 0
		        {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 :one
			 2 ]))
(def msg5c " compare unmodified vec-map-vec (map mid position)")

(def om5d (jsonZipper [ 0
		        {:a 1
			    :cc [1 2 3]
			    :b "test-string"}]))
(def msg5d " compare unmodified vec-map-vec (map tail position)")

(def om5e (jsonZipper {:cc [1 2 3]
		       :a 1
		       :b "test-string"}))
(def msg5e " compare unmodified map-vec (vec in head position)")

(def om5f (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def om5f1 (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def om5f2 (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def msg5f " compare unmodified map-vec (vec in mid position)")

(def om5g (jsonZipper {:a 1
		      :b "test-string"
		      :cc [1 2 3]}))
(def msg5g " compare unmodified map-vec (vec in tail position)")


(deftest test5 ;; 
  (discoverTest patch5empty om5a om5a  msg5a)
  (discoverTest patch5empty om5b om5b  msg5b)
  (discoverTest patch5empty om5c om5c  msg5c)
  (discoverTest patch5empty om5d om5d  msg5d)
  (discoverTest patch5empty om5e om5e  msg5e)
  (discoverTest patch5empty om5f om5f  msg5f)
  (discoverTest patch5empty om5f1 om5f2  msg5f)
  (discoverTest patch5empty om5g om5g  msg5g)
  )



;;;;;;;;;;;;;
;; test 6: Test-suite for detection of deletions in vectors
;;  (detecting a shift instead of set of changes to subsequent elements of the vector.)
;;


(def org6abcd (jsonZipper [1
			2]))

(def mod6a (jsonZipper [2]))

(def patch6a [(Patch. ["/"] actDelete "[0]" nil)])

(def msg6a "delete first field of vector")


(def mod6b (jsonZipper [1]))

(def patch6b [
	      (Patch. ["/"] actDelete "[1]" nil)])

(def msg6b "delete last field of vector")


(def mod6c (jsonZipper [1, 2, 3]))

(def patch6c [
	      (Patch. ["/"] actInsert "[2]" 3)])

(def msg6c "append (basic) value at end of vector")


(def mod6d (jsonZipper []))

(def patch6d [(Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[0]" nil)])

(def msg6d "delete all fields of vector")

(def org6efg (jsonZipper [1
			 2
			 3
			 4
			 5]))

(def mod6e (jsonZipper [1 5]))

(def patch6e [(Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)])

(def msg6e "delete fields at position 2-4")


(def mod6f (jsonZipper [1 3 5]))

(def patch6f [(Patch. ["/"] actDelete "[1]" nil)
	      (Patch. ["/"] actDelete "[2]" nil)])

(def msg6f "delete fields at position 2 and 4")

(def mod6g (jsonZipper [1 "changed" 5]))

(def patch6g [(Patch. ["/"] actChange "[1]" "changed")
	      (Patch. ["/"] actDelete "[2]"  nil)
	      (Patch. ["/"] actDelete "[2]" nil)])

(def msg6g "delete fields at position 2-4 and modified position 3")

(deftest test6 ;; 
  (discoverTest patch6a org6abcd mod6a  msg6a)
  (discoverTest patch6b org6abcd mod6b  msg6b)
  (discoverTest patch6c org6abcd mod6c  msg6c)
  (discoverTest patch6d org6abcd mod6d  msg6d)
  (discoverTest patch6e org6efg mod6e  msg6e)
  (discoverTest patch6f org6efg mod6f  msg6f)
  (discoverTest patch6g org6efg mod6g  msg6g)
  )

;; (deftest test6 ;; 
;;   (applyTest patch6a org6abcd mod6a  msg6a)
;;   (applyTest patch6b org6abcd mod6b  msg6b)
;;   (applyTest patch6c org6abcd mod6c  msg6c)
;;   (applyTest patch6d org6abcd mod6d  msg6d)
;;   (applyTest patch6e org6efg mod6e  msg6e)
;;   (applyTest patch6f org6efg mod6f  msg6f)
;;   (applyTest patch6g org6efg mod6g  msg6g)
;;   )

