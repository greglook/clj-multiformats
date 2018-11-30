(ns multiformats.base-test
  (:require
    [alphabase.bytes :refer [bytes=]]
    [clojure.test :refer [deftest testing is]]
    [multiformats.base :as mbase]))

; TODO: cljc


(deftest arg-validation
  (testing "encoding"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not encode empty data"
          (mbase/encode :base32 (byte-array 0))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":foo is not a supported multibase encoding"
          (mbase/encode :foo (byte-array 1)))))
  (testing "decoding"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is too short to be multibase-encoded data"
          (mbase/decode "")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is too short to be multibase-encoded data"
          (mbase/decode "1")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"prefix \\x does not map to a supported multibase encoding"
          (mbase/decode "xabc")))))


(deftest example-1
  (let [data (.getBytes "Decentralize everything!!")]
    (testing "base16"
      (is (= "f446563656e7472616c697a652065766572797468696e672121"
             (mbase/encode :base16 data)))
      (is (bytes= data (mbase/decode "f446563656e7472616c697a652065766572797468696e672121")))
      (is (bytes= data (mbase/decode "F446563656E7472616C697A652065766572797468696E672121"))))
    (testing "base32"
      (is (= "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"
             (mbase/encode :base32 data)))
      (is (bytes= data (mbase/decode "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb")))
      (is (bytes= data (mbase/decode "BIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"))))
    (testing "base32hex"
      (is (= "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"
             (mbase/encode :base32hex data)))
      (is (bytes= data (mbase/decode "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891")))
      (is (bytes= data (mbase/decode "V8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"))))
    (testing "base58btc"
      (is (= "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"
             (mbase/encode :base58btc data)))
      (is (bytes= data (mbase/decode "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"))))
    (testing "inspect"
      (let [info (mbase/inspect "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe")]
        (is (map? info))
        (is (= \z (:prefix info)))
        (is (= :base58btc (:base info)))))))
