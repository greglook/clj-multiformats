(ns multiformats.base-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    #?(:cljs [goog.crypt :as crypt])
    [multiformats.base :as mbase]))


(defn- string-bytes
  "Get a UTF-8 byte array from a string."
  [string]
  (#?(:clj .getBytes, :cljs crypt/stringToUtf8ByteArray) string))


(deftest arg-validation
  (testing "formatting"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"not format empty data"
          (mbase/format :base32 (b/byte-array 0))))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"foo does not have a supported multibase formatter"
          (mbase/format :foo (b/byte-array 1)))))
  (testing "parsing"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"is too short to be multibase-formatted data"
          (mbase/parse "")))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"is too short to be multibase-formatted data"
          (mbase/parse "1")))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"prefix \"x\" does not map to a supported multibase encoding"
          (mbase/parse "xabc")))))


(deftest example-1
  (let [data (string-bytes "Decentralize everything!!")]
    #_
    (testing "base2"
      (let [encoded "001000100011001010110001101100101011011100111010001110010011000010110110001101001011110100110010100100000011001010111011001100101011100100111100101110100011010000110100101101110011001110010000100100001"]
        (is (= encoded (mbase/format :base2 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base8"
      (let [encoded "71043126154533472162302661513646244031273145344745643206455631620441"]
        (is (= encoded (mbase/format :base8 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_
    (testing "base10"
      (let [encoded "9429328951066508984658627669258025763026247056774804621697313"]
        (is (= encoded (mbase/format :base10 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base16"
      (let [encoded "f446563656e7472616c697a652065766572797468696e672121"]
        (is (= encoded (mbase/format :base16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE16"
      (let [encoded "F446563656E7472616C697A652065766572797468696E672121"]
        (is (= encoded (mbase/format :BASE16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32"
      (let [encoded "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"]
        (is (= encoded (mbase/format :base32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32"
      (let [encoded "BIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"]
        (is (= encoded (mbase/format :BASE32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32pad"
      (let [encoded "cirswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"]
        (is (= encoded (mbase/format :base32pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32PAD"
      (let [encoded "CIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"]
        (is (= encoded (mbase/format :BASE32PAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32hex"
      (let [encoded "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"]
        (is (= encoded (mbase/format :base32hex data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32HEX"
      (let [encoded "V8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"]
        (is (= encoded (mbase/format :BASE32HEX data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32hexpad"
      (let [encoded "t8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"]
        (is (= encoded (mbase/format :base32hexpad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32HEXPAD"
      (let [encoded "T8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"]
        (is (= encoded (mbase/format :BASE32HEXPAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32z"
      (let [encoded "het1sg3mqqt3gn5djxj11y3msci3817depfzgqejb"]
        (is (= encoded (mbase/format :base32z data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base58flickr"
      (let [encoded "Ztwe7gVTeK8wswS1gf8hrgAua9fcw9reboD"]
        (is (= encoded (mbase/format :base58flickr data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base58btc"
      (let [encoded "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"]
        (is (= encoded (mbase/format :base58btc data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64"
      (let [encoded "mRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"]
        (is (= encoded (mbase/format :base64 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64pad"
      (let [encoded "MRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="]
        (is (= encoded (mbase/format :base64pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64url"
      (let [encoded "uRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"]
        (is (= encoded (mbase/format :base64url data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64urlpad"
      (let [encoded "URGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="]
        (is (= encoded (mbase/format :base64urlpad data)))
        (is (bytes= data (mbase/parse encoded)))))))


(deftest example-2
  (let [data (string-bytes "yes mani !")]
    #_
    (testing "base2"
      (let [encoded "001111001011001010111001100100000011011010110000101101110011010010010000000100001"]
        (is (= encoded (mbase/format :base2 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base8"
      (let [encoded "7171312714403326055632220041"]
        (is (= encoded (mbase/format :base8 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_
    (testing "base10"
      (let [encoded "9573277761329450583662625"]
        (is (= encoded (mbase/format :base10 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base16"
      (let [encoded "f796573206d616e692021"]
        (is (= encoded (mbase/format :base16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE16"
      (let [encoded "F796573206D616E692021"]
        (is (= encoded (mbase/format :BASE16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32"
      (let [encoded "bpfsxgidnmfxgsibb"]
        (is (= encoded (mbase/format :base32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32"
      (let [encoded "BPFSXGIDNMFXGSIBB"]
        (is (= encoded (mbase/format :BASE32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32pad"
      (let [encoded "cpfsxgidnmfxgsibb"]
        (is (= encoded (mbase/format :base32pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32PAD"
      (let [encoded "CPFSXGIDNMFXGSIBB"]
        (is (= encoded (mbase/format :BASE32PAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32hex"
      (let [encoded "vf5in683dc5n6i811"]
        (is (= encoded (mbase/format :base32hex data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32HEX"
      (let [encoded "VF5IN683DC5N6I811"]
        (is (= encoded (mbase/format :BASE32HEX data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32hexpad"
      (let [encoded "tf5in683dc5n6i811"]
        (is (= encoded (mbase/format :base32hexpad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32HEXPAD"
      (let [encoded "TF5IN683DC5N6I811"]
        (is (= encoded (mbase/format :BASE32HEXPAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32z"
      (let [encoded "hxf1zgedpcfzg1ebb"]
        (is (= encoded (mbase/format :base32z data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base58flickr"
      (let [encoded "Z7Pznk19XTTzBtx"]
        (is (= encoded (mbase/format :base58flickr data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base58btc"
      (let [encoded "z7paNL19xttacUY"]
        (is (= encoded (mbase/format :base58btc data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64"
      (let [encoded "meWVzIG1hbmkgIQ"]
        (is (= encoded (mbase/format :base64 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64pad"
      (let [encoded "MeWVzIG1hbmkgIQ=="]
        (is (= encoded (mbase/format :base64pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64url"
      (let [encoded "ueWVzIG1hbmkgIQ"]
        (is (= encoded (mbase/format :base64url data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64urlpad"
      (let [encoded "UeWVzIG1hbmkgIQ=="]
        (is (= encoded (mbase/format :base64urlpad data)))
        (is (bytes= data (mbase/parse encoded)))))))


#_ ; FIXME: various broken bases
(deftest example-3
  (let [data (string-bytes "hello world")]
    #_
    (testing "base2"
      (let [encoded "00110100001100101011011000110110001101111001000000111011101101111011100100110110001100100"]
        (is (= encoded (mbase/format :base2 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base8"
      (let [encoded "7064145330661571007355734466144"]
        (is (= encoded (mbase/format :base8 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_
    (testing "base10"
      (let [encoded "9126207244316550804821666916"]
        (is (= encoded (mbase/format :base10 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base16"
      (let [encoded "f68656c6c6f20776f726c64"]
        (is (= encoded (mbase/format :base16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE16"
      (let [encoded "F68656C6C6F20776F726C64"]
        (is (= encoded (mbase/format :BASE16 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32"
      (let [encoded "bnbswy3dpeb3w64tmmq"]
        (is (= encoded (mbase/format :base32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32"
      (let [encoded "BNBSWY3DPEB3W64TMMQ"]
        (is (= encoded (mbase/format :BASE32 data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32pad"
      (let [encoded "cnbswy3dpeb3w64tmmq======"]
        (is (= encoded (mbase/format :base32pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32PAD"
      (let [encoded "CNBSWY3DPEB3W64TMMQ======"]
        (is (= encoded (mbase/format :BASE32PAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base32hex"
      (let [encoded "vd1imor3f41rmusjccg"]
        (is (= encoded (mbase/format :base32hex data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "BASE32HEX"
      (let [encoded "VD1IMOR3F41RMUSJCCG"]
        (is (= encoded (mbase/format :BASE32HEX data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32hexpad"
      (let [encoded "td1imor3f41rmusjccg======"]
        (is (= encoded (mbase/format :base32hexpad data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "BASE32HEXPAD"
      (let [encoded "TD1IMOR3F41RMUSJCCG======"]
        (is (= encoded (mbase/format :BASE32HEXPAD data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base32z"
      (let [encoded "hpb1sa5dxrb5s6hucco"]
        (is (= encoded (mbase/format :base32z data)))
        (is (bytes= data (mbase/parse encoded)))))
    #_ ; TODO: implement
    (testing "base58flickr"
      (let [encoded "ZrTu1dk6cWsRYjYu"]
        (is (= encoded (mbase/format :base58flickr data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base58btc"
      (let [encoded "zStV1DL6CwTryKyV"]
        (is (= encoded (mbase/format :base58btc data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64"
      (let [encoded "maGVsbG8gd29ybGQ"]
        (is (= encoded (mbase/format :base64 data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64pad"
      (let [encoded "MaGVsbG8gd29ybGQ="]
        (is (= encoded (mbase/format :base64pad data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64url"
      (let [encoded "uaGVsbG8gd29ybGQ"]
        (is (= encoded (mbase/format :base64url data)))
        (is (bytes= data (mbase/parse encoded)))))
    (testing "base64urlpad"
      (let [encoded "UaGVsbG8gd29ybGQ="]
        (is (= encoded (mbase/format :base64urlpad data)))
        (is (bytes= data (mbase/parse encoded)))))))


#_ ; FIXME: broken tests
(deftest mixed-case
  (let [data (string-bytes "hello world")]
    (is (bytes= data (mbase/parse "f68656c6c6f20776F726C64")))
    (is (bytes= data (mbase/parse "F68656c6c6f20776F726C64")))
    (is (bytes= data (mbase/parse "bnbswy3dpeB3W64TMMQ")))
    (is (bytes= data (mbase/parse "Bnbswy3dpeB3W64TMMQ")))
    (is (bytes= data (mbase/parse "vd1imor3f41RMUSJCCG")))
    (is (bytes= data (mbase/parse "Vd1imor3f41RMUSJCCG")))
    #_(is (bytes= data (mbase/parse "cnbswy3dpeB3W64TMMQ======")))
    #_(is (bytes= data (mbase/parse "Cnbswy3dpeB3W64TMMQ======")))
    #_(is (bytes= data (mbase/parse "td1imor3f41RMUSJCCG======")))
    #_(is (bytes= data (mbase/parse "Td1imor3f41RMUSJCCG======")))))


(deftest miscellaneous
  (testing "inspect"
    (let [info (mbase/inspect "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe")]
      (is (map? info))
      (is (= "z" (:prefix info)))
      (is (= :base58btc (:base info))))))
