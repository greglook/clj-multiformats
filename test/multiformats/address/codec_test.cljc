(ns multiformats.address.codec-test
  (:require
   [alphabase.bytes :as b :refer [bytes= byte-seq]]
   [multiformats.address.codec :as codec]
   [clojure.string :as str]
   #?(:clj [clojure.test :refer [deftest testing is are]]
      :cljs [cljs.test :refer-macros [deftest testing is are]]))
  #?(:clj
     (:import
      clojure.lang.ExceptionInfo)))

(defn ->bytes [bs]
  (let [dst (b/byte-array (count bs))]
    (doseq [[idx b] (map-indexed vector bs)]
      (b/set-byte dst idx b))
    dst))

(defn roundtrip [codec x]
  (->> x
       (codec/str->bytes codec)
       (codec/bytes->str codec)))

(deftest ip-test
  (testing "parsing address"
    (are [ip-codec expected addr]
        (= expected (b/byte-seq (codec/str->bytes ip-codec addr)))
      codec/ip4-codec [127 0 0 1] "127.0.0.1"
      codec/ip4-codec [180 10 1 5] "180.10.1.5"
      codec/ip6-codec [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1] "::1"
      codec/ip6-codec [32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52] "2001:db8:85a3:0:0:8a2e:370:7334"))

  (testing "address to bytes"
    (are [ip-codec addr-bytes-seq expected]
        (let [addr-bytes (->bytes addr-bytes-seq)]
          (is (= expected (codec/bytes->str ip-codec addr-bytes))))
      codec/ip4-codec [127 0 0 1] "127.0.0.1"
      codec/ip4-codec [180 10 1 5] "180.10.1.5"
      codec/ip6-codec [32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52] "2001:db8:85a3:0:0:8a2e:370:7334"))

  (testing "round trip on address string"
    (are [codec addr]
      (= addr (roundtrip codec addr))
      codec/ip4-codec "127.0.0.1"
      codec/ip4-codec "180.10.1.5"
      codec/ip6-codec "0:0:0:0:0:0:0:1"
      codec/ip6-codec "2001:db8:85a3:0:0:8a2e:370:7334"))

  (testing "format validation"
    (are [codec bad-addr]
        (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
                 (codec/str->bytes codec bad-addr))
      codec/ip4-codec "266.0.0.1"
      codec/ip4-codec "266.0.0"
      codec/ip4-codec "127.0:0.0.1")))

(deftest uint-codec-test
  (testing "round trip on numbers"
    (are [codec n]
      (= n (roundtrip codec/utf8-codec n))
      codec/ushort-codec "80"
      codec/ushort-codec "128"
      (codec/->UnsignedNumTranscoder 10 1) "100"))

  (testing "format value to bytes"
    (are [codec bad-num]
      (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
               (codec/str->bytes codec bad-num))
      codec/ushort-codec "70000"
      (codec/->UnsignedNumTranscoder 10 1) "256"))


  (testing "parsing bytes validation"
    (are [codec bad-bytes]
        (thrown? #?(:clj ExceptionInfo, :cljs js/Error)
                 (codec/bytes->str codec (->bytes bad-bytes)))
      codec/ushort-codec [0 0 0]
      codec/ushort-codec [0])))

(deftest utf8-codec
  (testing "roundtrip"
    (are [hostname]
      (= hostname (roundtrip codec/utf8-codec hostname))
      "google.com"
      "xyz.com/blah.html")))
