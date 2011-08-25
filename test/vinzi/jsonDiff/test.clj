(ns vinzi.jsonDiff.test
  (:use [vinzi.jsonDiff] :reload)
  (:import [vinzi.jsonDiff Patch])
;;  (:require  [clojure.contrib
;;	      [json :as json]
;;	      [duck-streams :as ds :only [writer]]
	      ;;	      [sql :as sql]
;;	      ])
  (:use [vinzi.jsonZip :only [jsonZipper]])
  (:use [clojure.test]))


(defn- strComp [x y]
  ;; (print "Comparing x = ")
  ;; (pprint x)
  ;; (print "     with y = ")
  ;; (pprint y)
  (= 0 (compare (str x) (str y))))


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


(defmacro theTest [patch org mod msg]
  `(is (strComp ~patch (findPatchesZipper ~org ~mod))  (str " DISCOVER-TEST:" ~msg)))

(deftest test1 ;; 
  (theTest patch1a org1 mod1a  msg1a)
  (theTest patch1b org1 mod1b  msg1b)
  (theTest patch1c org1 mod1c  msg1c)
  (theTest patch1d org1 mod1d  msg1d)
  (theTest patch1e org1 mod1e  msg1e)
  (theTest patch1f org1 mod1f  msg1f)
  (theTest patch1g org1 mod1g  msg1g)
  (theTest patch1h org1 mod1h  msg1h)
  )

(deftest test1reverse   ;; apply patches and see whether you get the original
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
  (theTest patch2a org2 mod2a  msg2a)
  (theTest patch2b org2 mod2b  msg2b)
  (theTest patch2c org2 mod2c  msg2c)
  )



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
  (theTest patch3a org3 mod3a  msg3a)
  (theTest patch3b org3 mod3b  msg3b)
  (theTest patch3c org3 mod3c  msg3c)
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
  (theTest patch4a org4 mod4a  msg4a)
  (theTest patch4b org4 mod4b  msg4b)
  (theTest patch4c org4 mod4c  msg4c)
  (theTest patch4d org4d mod4d  msg4d)
  (theTest patch4e org4e mod4e  msg4e)
  (theTest patch4f org4f mod4f  msg4f)
  )


;;;;;;;;;;;;;
;; test 5: some more tests on records containing vectors.
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


(def org5hij (jsonZipper [1
			2]))

(def mod5h (jsonZipper [2]))

(def patch5h [(Patch. ["/"] actChange "[0]" 2)
	      (Patch. ["/"] actDelete "[1]" nil)])

(def msg5h "delete first field of vector")


(def mod5i (jsonZipper [1]))

(def patch5i [
	      (Patch. ["/"] actDelete "[1]" nil)])

(def msg5i "delete last field of vector")


(def mod5j (jsonZipper [1, 2, 3]))

(def patch5j [
	      (Patch. ["/"] actInsert "[2]" 3)])

(def msg5j "append (basic) value at end of vector")


(def mod5k (jsonZipper []))

(def patch5k [(Patch. ["/"] actDelete "[0]" nil)
	      (Patch. ["/"] actDelete "[1]" nil)])

(def msg5k "delete last field of vector")


;(def {:a 1, :cc [1 2 3], :b "test-string"}

(deftest test5 ;; 
  (theTest patch5empty om5a om5a  msg5a)
  (theTest patch5empty om5b om5b  msg5b)
  (theTest patch5empty om5c om5c  msg5c)
  (theTest patch5empty om5d om5d  msg5d)
  (theTest patch5empty om5e om5e  msg5e)
  (theTest patch5empty om5f om5f  msg5f)
  (theTest patch5empty om5f1 om5f2  msg5f)
  (theTest patch5empty om5g om5g  msg5g)
  (theTest patch5h org5hij mod5h  msg5h)
  (theTest patch5i org5hij mod5i  msg5i)
  (theTest patch5j org5hij mod5j  msg5j)
  (theTest patch5k org5hij mod5k  msg5k)
  )

