(ns multiformats.cid-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.test :refer [deftest testing is]]
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


(deftest predicate
  (is (cid/cid? (cid/create :raw (mhash/sha1 "hello"))))
  (is (not (cid/cid? nil)))
  (is (not (cid/cid? "foo"))))


(deftest value-semantics
  (let [mh (mhash/sha1 "hello world")
        a (cid/create :raw mh)
        b (cid/create 0x51 mh)
        b' (cid/create :cbor mh)]
    (testing "equality"
      (is (= a a) "identical values are equal")
      (is (= b b') "values with same code and digest are equal")
      (is (not= a b))
      (is (not= a nil))
      (is (not= a "foo")))
    (testing "hashing"
      (is (integer? (hash a)) "hash code returns an integer")
      (is (= (hash a) (hash a)) "hash code is reflexive")
      (is (= (hash b) (hash b')) "equivalent objects return same hashcode"))
    (testing "comparison"
      (is (zero? (compare a a)))
      (is (zero? (compare b b')))
      (is (pos? (compare a b)))
      (is (neg? (compare b a)))
      (is (thrown-with-msg? #?(:clj ExceptionInfo, :cljs js/Error) #"Cannot compare CID value to"
            (compare a "foo"))))))


(deftest cid-properties
  (let [mh (mhash/sha2-256 "test input")
        cid (cid/create :cbor mh)]
    (is (= 36 (:length cid)))
    (is (= 1 (:version cid)))
    (is (= :cbor (:codec cid)))
    (is (= 0x51 (:code cid)))
    (is (= :sha2-256 (get-in cid [:hash :algorithm])))
    (is (nil? (:foo cid)))
    (is (= {:length 36
            :version 1
            :codec :cbor
            :code 0x51
            :hash mh}
           (cid/inspect cid)))
    (is (nil? (cid/inspect nil)))))


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


(deftest binary-serialization
  (testing "v0"
    (let [mh (mhash/sha2-256 "hello world")
          encoded (mhash/encode mh)
          cid (cid/decode encoded)]
      (is (= 0 (:version cid)))
      (is (= :raw (:codec cid)))
      (is (= mh (:hash cid)))
      (is (bytes= encoded (cid/encode cid)))
      (let [buffer (b/byte-array (+ 4 (:length cid)))]
        (is (= (:length cid) (cid/write-bytes cid buffer 2)))
        (is (= [0x00 0x00 0x12 0x20]
               (take 4 (b/byte-seq buffer)))))))
  (testing "v1"
    (let [mh (mhash/sha2-256 "hello world")
          cid (cid/create :cbor mh)]
      (is (= 1 (:version cid)))
      (is (= :cbor (:codec cid)))
      (is (= mh (:hash cid)))
      (let [encoded (cid/encode cid)]
        (is (= (alength encoded) (:length cid)))
        (is (= cid (cid/decode encoded))))
      (let [buffer (b/byte-array (+ 4 (:length cid)))]
        (is (= (:length cid) (cid/write-bytes cid buffer 1)))
        (is (= [0x00 0x01 0x51 0x12 0x20]
               (take 5 (b/byte-seq buffer)))))))
  (testing "bad input"
    (let [encoded (doto (b/byte-array 34)
                    (b/set-byte 0 0x02))]
      (is (thrown-with-msg? #?(:clj ExceptionInfo, :cljs js/Error)
                            #"Unable to decode CID version 2"
            (cid/decode encoded))))))


(deftest string-serialization
  (testing "v0"
    (let [b58 "Qmd8kgzaFLGYtTS1zfF37qKGgYQd5yKcQMyBeSa8UkUz4W"
          mh (mhash/sha2-256 "foo bar baz")
          encoded (mhash/encode mh)
          cid (cid/decode encoded)]
      (is (= 34 (:length cid)))
      (is (= 0 (:version cid)))
      (is (= :raw (:codec cid)))
      (is (= b58 (cid/format cid)))
      (is (= cid (cid/parse b58)))
      (is (thrown-with-msg? #?(:clj ExceptionInfo, :cljs js/Error)
                            #"v0 CID values cannot be formatted in alternative bases"
            (cid/format :base32 cid)))
      (is (= {:length 34
              :version 0
              :codec :raw
              :code 0x55
              :hash mh}
             (cid/inspect cid)))
      (is (= {:length 34
              :version 0
              :codec :raw
              :code 0x55
              :hash mh}
             (cid/inspect encoded)))
      (is (= {:length 34
              :version 0
              :codec :raw
              :code 0x55
              :hash mh}
             (cid/inspect b58)))))
  (testing "v1"
    (let [b58 "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA"
          b32 "bafkreidon73zkcrwdb5iafqtijxildoonbwnpv7dyd6ef3qdgads2jc4su"
          cid (cid/parse b58)
          mh (:hash cid)]
      (is (= 36 (:length cid)))
      (is (= 1 (:version cid)))
      (is (= :raw (:codec cid)))
      (is (= b58 (cid/format :base58btc cid)))
      (is (= b32 (cid/format cid)))
      (is (= cid (cid/parse b32)))
      (is (= {:length 36
              :version 1
              :codec :raw
              :code 0x55
              :hash mh
              :prefix "b"
              :base :base32}
             (cid/inspect b32)))
      (is (= {:length 36
              :version 1
              :codec :raw
              :code 0x55
              :hash mh
              :prefix "z"
              :base :base58btc}
             (cid/inspect b58))))))
