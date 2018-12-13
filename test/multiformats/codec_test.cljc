(ns multiformats.codec-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    [multiformats.codec :as mcodec]))


; TODO: register-codec


(deftest resolution
  (testing "resolve-key"
    (is (= :raw (mcodec/resolve-key 0x55)))
    (is (= :cbor (mcodec/resolve-key :cbor)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"codes cannot be negative"
          (mcodec/resolve-key -1)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #":foo does not map to a known multicodec"
          (mcodec/resolve-key :foo)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"is not a valid codec keyword or numeric code"
          (mcodec/resolve-key "abc"))))
  (testing "resolve-code"
    (is (= 0x55 (mcodec/resolve-code 0x55)))
    (is (= 0x51 (mcodec/resolve-code :cbor)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"codes cannot be negative"
          (mcodec/resolve-code -1)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #":foo does not map to a known multicodec"
          (mcodec/resolve-code :foo)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"is not a valid codec keyword or numeric code"
          (mcodec/resolve-code "abc")))))
