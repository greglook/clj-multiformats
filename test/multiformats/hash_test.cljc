(ns multiformats.hash-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    [multiformats.base.b16 :as hex]
    [multiformats.hash :as mhash]))


(deftest constructor-validation
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs js/Error)
               (mhash/create :no-such-algo (hex/parse "0beec7b8")))
      "Unknown algorith keyword should be rejected")
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs js/Error)
               (mhash/create :sha1 nil))
      "Nil digest should be rejected")
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs js/Error)
               (mhash/create :sha1 (b/byte-array 0)))
      "Empty digest should be rejected"))


(deftest value-semantics
  (let [a (mhash/create 0x11 (hex/parse "0beec7b8"))
        b (mhash/create 0x11 (hex/parse "94a1be0c"))
        c (mhash/create 0x12 (hex/parse "00a8b94e"))
        c' (mhash/create 0x12 (hex/parse "00a8b94e"))]
    (is (= a a) "Identical values are equal")
    (is (= c c') "Values with same code and digest are equal")
    (is (integer? (hash b)) "Hash code returns an integer")
    (is (= (hash c) (hash c')) "Equivalent objects return same hashcode")
    (is (= [a b c] (sort [c b a])) "Multihashes sort in code/digest order")))


(deftest multihash-rendering
  (is (= "hash:sha1:0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
         (str (mhash/create :sha1 (hex/parse "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")))))
  (is (= "hash:sha2-256:2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
         (str (mhash/create :sha2-256 (hex/parse "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae")))))
  (is (= "hash:sha1:ea347f3c5b8f0fd07b5bc95d0beecdda3c275da3"
         (str (mhash/create :sha1 (hex/parse "ea347f3c5b8f0fd07b5bc95d0beecdda3c275da3"))))))


(deftest exercise-metadata
  (let [a (mhash/create :sha1 (hex/parse "dbc95275da8a3d0d0beeea3f0fd47f3cc7b55bc3"))
        a' (vary-meta a assoc :foo :bar/baz)]
    (is (empty? (meta a)) "values start with empty metadata")
    (is (= :bar/baz (:foo (meta a'))) "metadata can be associated with value")
    (is (= a a') "metadata does not affect equality")))


(def examples
  "Test case examples."
  {"11140beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
   [0x11 :sha1 160 "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"]

   "11040beec7b8"
   [0x11 :sha1 32 "0beec7b8"]

   "12202c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
   [0x12 :sha2-256 256 "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"]

   "12042c26b46b"
   [0x12 :sha2-256 32 "2c26b46b"]

   "22040006b46b"
   [0x22 :murmur3 32 "0006b46b"]})


(deftest example-coding
  (testing "Encoding is reflexive"
    (let [mhash (mhash/create 0x02 (hex/parse "0beec7b8"))]
      (is (= mhash (mhash/decode (mhash/encode mhash))))))
  (doseq [[hex [code algorithm bits digest]] examples]
    (let [mhash (mhash/create algorithm (hex/parse digest))]
      (is (= code (:code mhash)))
      (is (= algorithm (:algorithm mhash)))
      (is (= bits (:bits mhash)))
      (is (= digest (:digest mhash)))
      #_
      (is (= hex (mhash/hex mhash))
          "Encoded multihashes match expected hex")
      #_
      (is (= mhash (mhash/decode hex))
          "Hex decodes into expected multihash")
      #_
      (let [b58 (mhash/base58 mhash)]
        (is (string? b58) "Multihash encodes to a base-58 string")
        (is (= mhash (mhash/decode b58))
            "Multihash round-trips through Base58 encoding")))))
