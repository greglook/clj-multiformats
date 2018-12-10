(ns multiformats.cid-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    [multiformats.cid :as cid]
    [multiformats.hash :as mhash])
  #?(:clj
     (:import
       clojure.lang.ExceptionInfo)))


(deftest constructor-validation
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
               (cid/create true (mhash/sha1 "hello world")))
      "Unknown codec type should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
               (cid/create :no-such-codec (mhash/create :sha1 "0beec7b8")))
      "Unknown algorithm keyword should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
               (cid/create -1 (mhash/sha1 "hello world")))
      "Negative codec code should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
               (cid/create :cbor nil))
      "Nil hash should be rejected"))


(deftest value-semantics
  (let [mh (mhash/sha1 "hello world")
        a (cid/create :raw mh)
        b (cid/create 0x51 mh)
        b' (cid/create :cbor mh)]
    (is (= a a) "identical values are equal")
    (is (= b b') "values with same code and digest are equal")
    (is (integer? (hash a)) "hash code returns an integer")
    (is (= (hash b) (hash b')) "equivalent objects return same hashcode")))


(deftest cid-properties
  (let [cid (cid/create :cbor (mhash/sha2-256 "test input"))]
    (is (= 36 (:length cid)))
    (is (= 1 (:version cid)))
    (is (= :cbor (:codec cid)))
    (is (= 0x51 (:code cid)))
    (is (= :sha2-256 (get-in cid [:hash :algorithm])))
    (is (nil? (:foo cid)))))


(deftest cid-rendering
  (is (= "cidv1:raw:sha1:0beec7b5ea3f0fdb"
         (str (cid/create :raw (mhash/create :sha1 "0beec7b5ea3f0fdb")))))
  (is (= "cidv1:cbor:sha2-256:2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
         (str (cid/create :cbor (mhash/create :sha2-256 "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"))))))


(deftest exercise-metadata
  (let [a (cid/create :raw (mhash/sha2-256 "abc123"))
        a' (vary-meta a assoc :foo :bar/baz)]
    (is (empty? (meta a)) "values start with empty metadata")
    (is (= :bar/baz (:foo (meta a'))) "metadata can be associated with value")
    (is (= a a') "metadata does not affect equality")))
