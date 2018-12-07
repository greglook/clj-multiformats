(ns multiformats.base-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    #?(:cljs [goog.crypt :as crypt])
    [multiformats.base :as mbase]))


(deftest base-setup
  #?(:clj
     (testing "install-base"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"registered with invalid key"
             (#'mbase/install-base {} {:key 123})))
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"has no assigned prefix code"
             (#'mbase/install-base {} {:key :foo})))
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is already registered"
             (#'mbase/install-base {:base1 {}} {:key :base1})))
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not specify a formatter function"
             (#'mbase/install-base {} {:key :base1})))
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not specify a parser function"
             (#'mbase/install-base {} {:key :base1, :formatter identity}))))))


(deftest arg-validation
  (testing "formatting"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"must be a keyword"
          (mbase/format* 123 (b/byte-array 1))))
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
                          #"must be a keyword"
          (mbase/parse* 123 "abc")))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"foo does not have a supported multibase parser"
          (mbase/parse* :foo "abc")))
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


(defn- string-bytes
  "Get a UTF-8 byte array from a string."
  [string]
  (#?(:clj .getBytes, :cljs crypt/stringToUtf8ByteArray) string))


(deftest example-1
  (let [data (string-bytes "Decentralize everything!!")
        cases {:base2 "001000100011001010110001101100101011011100111010001110010011000010110110001101001011110100110010100100000011001010111011001100101011100100111100101110100011010000110100101101110011001110010000100100001"
               :base8 "71043126154533472162302661513646244031273145344745643206455631620441"
               ;:base10 "9429328951066508984658627669258025763026247056774804621697313"
               :base16 "f446563656e7472616c697a652065766572797468696e672121"
               :BASE16 "F446563656E7472616C697A652065766572797468696E672121"
               :base32 "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"
               :BASE32 "BIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"
               :base32pad "cirswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"
               :BASE32PAD "CIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"
               :base32hex "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"
               :BASE32HEX "V8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"
               :base32hexpad "t8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"
               :BASE32HEXPAD "T8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"
               ;:base32z "het1sg3mqqt3gn5djxj11y3msci3817depfzgqejb"
               ;:base58flickr "Ztwe7gVTeK8wswS1gf8hrgAua9fcw9reboD"
               :base58btc "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"
               :base64 "mRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"
               :base64pad "MRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="
               :base64url "uRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"
               :base64urlpad "URGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="}]
    (doseq [[base encoded] cases]
      (testing (name base)
        (is (= encoded (mbase/format base data)))
        (is (bytes= data (mbase/parse encoded)))))))


(deftest example-2
  (let [data (string-bytes "yes mani !")
        cases {:base2 "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
               :base8 "7171312714403326055632220041"
               ;:base10 "9573277761329450583662625"
               :base16 "f796573206d616e692021"
               :BASE16 "F796573206D616E692021"
               :base32 "bpfsxgidnmfxgsibb"
               :BASE32 "BPFSXGIDNMFXGSIBB"
               :base32pad "cpfsxgidnmfxgsibb"
               :BASE32PAD "CPFSXGIDNMFXGSIBB"
               :base32hex "vf5in683dc5n6i811"
               :BASE32HEX "VF5IN683DC5N6I811"
               :base32hexpad "tf5in683dc5n6i811"
               :BASE32HEXPAD "TF5IN683DC5N6I811"
               ;:base32z "hxf1zgedpcfzg1ebb"
               ;:base58flickr "Z7Pznk19XTTzBtx"
               :base58btc "z7paNL19xttacUY"
               :base64 "meWVzIG1hbmkgIQ"
               :base64pad "MeWVzIG1hbmkgIQ=="
               :base64url "ueWVzIG1hbmkgIQ"
               :base64urlpad "UeWVzIG1hbmkgIQ=="}]
    (doseq [[base encoded] cases]
      (testing (name base)
        (is (= encoded (mbase/format base data)))
        (is (bytes= data (mbase/parse encoded)))))))


(deftest example-3
  (let [data (string-bytes "hello world")
        cases {:base2 "00110100001100101011011000110110001101111001000000111011101101111011100100110110001100100"
               :base8 "764145330661571007355734466144" ; originally had leading zero?
               ;:base10 "9126207244316550804821666916"
               :base16 "f68656c6c6f20776f726c64"
               :BASE16 "F68656C6C6F20776F726C64"
               :base32 "bnbswy3dpeb3w64tmmq"
               :BASE32 "BNBSWY3DPEB3W64TMMQ"
               :base32pad "cnbswy3dpeb3w64tmmq======"
               :BASE32PAD "CNBSWY3DPEB3W64TMMQ======"
               :base32hex "vd1imor3f41rmusjccg"
               :BASE32HEX "VD1IMOR3F41RMUSJCCG"
               :base32hexpad "td1imor3f41rmusjccg======"
               :BASE32HEXPAD "TD1IMOR3F41RMUSJCCG======"
               ;:base32z "hpb1sa5dxrb5s6hucco"
               ;:base58flickr "ZrTu1dk6cWsRYjYu"
               :base58btc "zStV1DL6CwTryKyV"
               :base64 "maGVsbG8gd29ybGQ"
               :base64pad "MaGVsbG8gd29ybGQ="
               :base64url "uaGVsbG8gd29ybGQ"
               :base64urlpad "UaGVsbG8gd29ybGQ="}]
    (doseq [[base encoded] cases]
      (testing (name base)
        (is (= encoded (mbase/format base data)))
        (is (bytes= data (mbase/parse encoded)))))))


(deftest mixed-case
  (let [data (string-bytes "hello world")]
    (testing "base16 vs BASE16"
      (is (bytes= data (mbase/parse "f68656c6c6f20776F726C64")))
      (is (bytes= data (mbase/parse "F68656c6c6f20776F726C64"))))
    (testing "base32 vs BASE32"
      (is (bytes= data (mbase/parse "bnbswy3dpeB3W64TMMQ")))
      (is (bytes= data (mbase/parse "Bnbswy3dpeB3W64TMMQ"))))
    (testing "base32hex vs BASE32HEX"
      (is (bytes= data (mbase/parse "vd1imor3f41RMUSJCCG")))
      (is (bytes= data (mbase/parse "Vd1imor3f41RMUSJCCG"))))
    (testing "base32pad vs BASE32PAD"
      (is (bytes= data (mbase/parse "cnbswy3dpeB3W64TMMQ======")))
      (is (bytes= data (mbase/parse "Cnbswy3dpeB3W64TMMQ======"))))
    (testing "base32hexpad vs BASE32HEXPAD"
      (is (bytes= data (mbase/parse "td1imor3f41RMUSJCCG======")))
      (is (bytes= data (mbase/parse "Td1imor3f41RMUSJCCG======"))))))


(deftest miscellaneous
  (testing "inspect"
    (let [info (mbase/inspect "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe")]
      (is (map? info))
      (is (= "z" (:prefix info)))
      (is (= :base58btc (:base info))))))
