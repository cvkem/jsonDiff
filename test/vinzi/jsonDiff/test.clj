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



;; test cases 1:  changing of basic key (at top level)

(def org1 (jsonZipper {:a 1
	   :b "test-string"}))

;; test 1a
(def mod1a (jsonZipper {:a 1
	   :b "mod-string"}))

(def patch1a [
	     (vinzi.jsonDiff.Patch. ["/"] actChange :b "mod-string")])

(def msg1a "TEST: change of string at top level")
     
;; test 1b
(def mod1b (jsonZipper {:a "1"
	   :b "test-string"}))

(def patch1b [
	     (Patch.  ["/"] actChange :a "1")])

(def msg1b "TEST: change of int to String at top level")
     
;; test 1c
(def mod1c (jsonZipper {:a 1}))

(def patch1c [
	      (Patch. ["/"] actDelete :b nil)])

(def msg1c "TEST: delete last field of hashmap")

;; test 1d
(def mod1d (jsonZipper {:a 1
			:b "test-string"
			:c "inserted :c"}))

(def patch1d [
	     (Patch. ["/"] actInsert :c "inserted :c")])

(def msg1d "TEST: added field at tail position")

;; test 1e
(def mod1e (jsonZipper {:a 1
			:b "test-string"
			:aa "inserted :a1"}))

(def patch1e [
	     (Patch. ["/"] actInsert :aa "inserted :a1")])

(def msg1e "TEST: added field at tail position")


(deftest test1 ;; 
  (is (strComp patch1a (findPatchesZipper org1 mod1a))  msg1a)
  (is (strComp patch1b (findPatchesZipper org1 mod1b))  msg1b)
  (is (strComp patch1c (findPatchesZipper org1 mod1c))  msg1c)
  (is (strComp patch1d (findPatchesZipper org1 mod1d))  msg1d)
  (is (strComp patch1e (findPatchesZipper org1 mod1e))  msg1e)
  )




;; test cases 2:  changing of compound key (at top level)


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

(def msg2a "TEST: add a compound key at top level")

;; test 2b
(def mod2b (jsonZipper
	    {:a 1
	     :cc {:x 1}
	     :b "test-string"}))

(def patch2b [
	     (Patch. ["/"] actInsert :cc {:x 1} )])

(def msg2b "TEST: add a compound key at top level tail position")

;; test 2c
(def mod2cdata  {:a 1
		 :cc [1 2 3]
		 :b "test-string"})
(def mod2c (jsonZipper mod2cdata))


(def patch2c [
	      (Patch. ["/"] actInsert :cc [1 2 3] )])

(def msg2c "TEST: add a compound key at top level tail position")


(deftest test2 ;; 
  (is (strComp patch2a (findPatchesZipper org2 mod2a))  msg2a)
  (is (strComp patch2b (findPatchesZipper org2 mod2b))  msg2b)
  (is (strComp patch2c (findPatchesZipper org2 mod2c))  msg2c)
  )




;; test cases 3:   adding keys at a level deeper in map


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

(def msg3a "TEST: add a compound key at second level within map")

;; test 3b
(def mod3b (jsonZipper
	    {:x {:a 1
		 :cc {:x 1}
		 :b "test-string"}}))

(def patch3b [
	     (Patch. ["/" :x] actInsert :cc {:x 1} )])

(def msg3b "TEST: add a compound key at second level of map at tail position")

;; test 3c
(def mod3cdata  {:x {:a 1
		     :cc [1 2 3]
		     :b "test-string"}})
(def mod3c (jsonZipper mod3cdata))


(def patch3c [
	      (Patch. ["/" :x] actInsert :cc [1 2 3] )])

(def msg3c "TEST: add a compound key at second level tail position")


(deftest test3 ;; 
  (is (strComp patch3a (findPatchesZipper org3 mod3a))  msg3a)
  (is (strComp patch3b (findPatchesZipper org3 mod3b))  msg3b)
  (is (strComp patch3c (findPatchesZipper org3 mod3c))  msg3c)
  )


;; test cases 4:   adding keys at second level deeper (nested in vector)


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

(def msg4a "TEST: add a compound key at second level within vector")

;; test 4b
(def mod4b (jsonZipper
	    [ 0
	      {:a 1
		 :cc {:x 1}
	       :b "test-string"}
	     2]))

(def patch4b [
	     (Patch. ["/" "[1]"] actInsert :cc {:x 1} )])

(def msg4b "TEST: add a compound key at second level of map at tail position")

;; test 4c
(def mod4cdata  [ 0
		 {:a 1
		     :cc [1 2 3]
		     :b "test-string"}
		  2 ])
(def mod4c (jsonZipper mod4cdata))


(def patch4c [
	      (Patch. ["/" "[1]"] actInsert :cc [1 2 3] )])

(def msg4c "TEST: add a compound key at second level tail position of map")

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

(def msg4d "TEST: add a compound key at second level at start of vector")

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

(def msg4e "TEST: add a compound key at second level at last position of a vector")


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

(def msg4f "TEST: add a compound key at position (vector contains both  numbers and keywords)")


(deftest test4 ;; 
  (is (strComp patch4a (findPatchesZipper org4 mod4a))  msg4a)
  (is (strComp patch4b (findPatchesZipper org4 mod4b))  msg4b)
  (is (strComp patch4c (findPatchesZipper org4 mod4c))  msg4c)
  (is (strComp patch4d (findPatchesZipper org4d mod4d))  msg4d)
  (is (strComp patch4e (findPatchesZipper org4e mod4e))  msg4e)
  (is (strComp patch4f (findPatchesZipper org4f mod4f))  msg4f)
  )



;; test 5: some more tests on records containing vectors.


;; test 5a
(def om5a (jsonZipper [1
			2]))
(def patch5empty [])
(def msg5a "TEST: compare vector to unchanged vector")


(def om5b (jsonZipper [ {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 :one
			 2 ]))
(def msg5b "TEST: compare unmodified vec-map-vec (map head position)")

(def om5c (jsonZipper [ 0
		        {:a 1
			    :cc [1 2 3]
			    :b "test-string"}
			 :one
			 2 ]))
(def msg5c "TEST: compare unmodified vec-map-vec (map mid position)")

(def om5d (jsonZipper [ 0
		        {:a 1
			    :cc [1 2 3]
			    :b "test-string"}]))
(def msg5d "TEST: compare unmodified vec-map-vec (map tail position)")

(def om5e (jsonZipper {:cc [1 2 3]
		       :a 1
		       :b "test-string"}))
(def msg5e "TEST: compare unmodified map-vec (vec in head position)")

(def om5f (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def om5f1 (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def om5f2 (jsonZipper {:a 1
		       :cc [1 2 3]
		       :b "test-string"}))
(def msg5f "TEST: compare unmodified map-vec (vec in mid position)")

(def om5g (jsonZipper {:a 1
		      :b "test-string"
		      :cc [1 2 3]}))
(def msg5g "TEST: compare unmodified map-vec (vec in tail position)")


;(def {:a 1, :cc [1 2 3], :b "test-string"}

(deftest test5 ;; 
  (is (strComp patch5empty (findPatchesZipper om5a om5a))  msg5a)
  (is (strComp patch5empty (findPatchesZipper om5b om5b))  msg5b)
  (is (strComp patch5empty (findPatchesZipper om5c om5c))  msg5c)
  (is (strComp patch5empty (findPatchesZipper om5d om5d))  msg5d)
  (is (strComp patch5empty (findPatchesZipper om5e om5e))  msg5e)
  (is (strComp patch5empty (findPatchesZipper om5f om5f))  msg5f)
  (is (strComp patch5empty (findPatchesZipper om5f1 om5f2))  msg5f)
  (is (strComp patch5empty (findPatchesZipper om5g om5g))  msg5f)
  )

