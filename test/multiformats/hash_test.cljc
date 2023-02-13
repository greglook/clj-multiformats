(ns multiformats.hash-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    #?(:cljs [goog.crypt :as crypt])
    [multiformats.hash :as mhash])
  #?(:clj
     (:import
       clojure.lang.ExceptionInfo
       java.io.ByteArrayInputStream
       java.nio.ByteBuffer)))


(deftest constructor-validation
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
        (mhash/create true "0beec7b8"))
      "Unknown algorithm type should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
        (mhash/create :no-such-algo "0beec7b8"))
      "Unknown algorithm keyword should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
        (mhash/create -1 "0beec7b8"))
      "Negative algorithm code should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
        (mhash/create :sha1 nil))
      "Nil digest should be rejected")
  (is (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
        (mhash/create 0x11 (b/byte-array 0)))
      "Empty digest should be rejected"))

#?(:bb nil :default
   (deftest value-semantics
     (let [a (mhash/create 0x11 "0beec7b8")
           b (mhash/create 0x11 "94a1be0c")
           c (mhash/create 0x12 "00a8b94e")
           c' (mhash/create 0x12 (b/init-bytes [0x00 0xa8 0xb9 0x4e]))]
       (is (= a a) "Identical values are equal")
       (is (= c c') "Values with same code and digest are equal")
       (is (integer? (hash b)) "Hash code returns an integer")
       (is (= (hash c) (hash c')) "Equivalent objects return same hashcode")
       #?(:bb nil
          :default (is (= [a b c] (sort [c b a])) "Multihashes sort in code/digest order")))))


(deftest multihash-rendering
  (is (= "hash:sha1:0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
         (str (mhash/create :sha1 "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"))))
  (is (= "hash:sha2-256:2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
         (str (mhash/create :sha2-256 "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"))))
  (is (= "hash:sha1:ea347f3c5b8f0fd07b5bc95d0beecdda3c275da3"
         (str (mhash/create :sha1 "ea347f3c5b8f0fd07b5bc95d0beecdda3c275da3")))))


(deftest exercise-metadata
  (let [a (mhash/create :sha1 "dbc95275da8a3d0d0beeea3f0fd47f3cc7b55bc3")
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

(def length #?(:bb mhash/length :default :length))
(def code* #?(:bb mhash/code :default :code))
(def algorithm* #?(:bb mhash/algorithm :default :algorithm))
(def digest* #?(:bb mhash/digest :default :digest))
(def bits* #?(:bb mhash/bits :default :bits))

(deftest example-coding
  (testing "Encoding is reflexive"
    (let [mhash (mhash/create 0x02 "0beec7b8")
          encoded (mhash/encode mhash)]
      (is (= 6 (length mhash) (alength encoded)))
      #?(:bb nil
         :default (is (= mhash (mhash/decode encoded))))))
  (testing "buffer writes"
    (let [mhash (mhash/create :sha1 "deadbeef")
          buffer (b/byte-array (+ 4 (length mhash)))]
      (is (= 6 (mhash/write-bytes mhash buffer 2)))
      (is (bytes= (b/init-bytes [0x00 0x00 0x11 0x04 0xde 0xad 0xbe 0xef 0x00 0x00])
                  buffer))))
  (doseq [[hex [code algorithm bits digest]] examples]
    (let [mhash (mhash/create algorithm digest)]
      (is (= (/ (count hex) 2) (length mhash)))
      (is (= code (code* mhash)))
      (is (= algorithm (algorithm* mhash)))
      (is (= bits (bits* mhash)))
      (is (= digest (digest* mhash)))
      (is (= hex (mhash/hex mhash))
          "Encoded multihashes match expected hex")
      #?(:bb nil
         :default
         (is (= mhash (mhash/parse hex))
             "Hex parses back to multihash")))))


(deftest hashing-constructors
  (doseq [[algorithm hash-fn] mhash/functions]
    (testing (str (name algorithm) " hashing")
      (let [content "foo bar baz"
            cbytes #?(:clj (.getBytes content)
                      :cljs (crypt/stringToUtf8ByteArray content))
            mh1 (hash-fn content)
            mh2 (hash-fn cbytes)
            mh3 #?(:clj (hash-fn (ByteBuffer/wrap (.getBytes content)))
                   :cljs mh1)
            mh4 #?(:clj (hash-fn (ByteArrayInputStream. (.getBytes content)))
                   :cljs mh1)]
        (is (= algorithm
               (algorithm* mh1)
               (algorithm* mh2)
               (algorithm* mh3)
               (algorithm* mh4))
            "Constructed multihash algorithms match")
        (is (= (digest* mh1)
               (digest* mh2)
               (digest* mh3)
               (digest* mh4))
            "Constructed multihash digests match")
        (is (thrown? #?(:clj Exception, :cljs js/Error)
              (hash-fn 123)))))))


(deftest content-validation
  (let [content "baz bar foo"
        mhash (mhash/sha1 content)]
    (is (nil? (mhash/test nil nil)))
    (is (nil? (mhash/test nil content)))
    (is (nil? (mhash/test mhash nil)))
    (is (true? (mhash/test mhash content))
        "Correct multihash returns true")
    (is (false? (mhash/test
                  (mhash/create :sha1 "68a9f54521a5501230e9dc73")
                  content))
        "Incorrect multihash returns false")
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"No supported hashing function for algorithm :shake-128"
          (mhash/test
            (mhash/create :shake-128 "68a9f54521a5501230e9dc73")
            content))
        "Unsupported hash function cannot be validated")))
