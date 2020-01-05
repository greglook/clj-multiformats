(ns multiformats.codec-test
  (:require
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    [multiformats.codec :as mcodec]))


(deftest codec-registration
  (testing "registration"
    (is (nil? (mcodec/unregister! :baz)))
    (is (nil? (mcodec/key->code :baz)))
    (is (nil? (mcodec/register! :baz 0x1234)))
    (is (= 0x1234 (mcodec/key->code :baz)))
    (is (= 0x1234 (mcodec/resolve-code :baz))))
  (testing "checks"
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"Invalid arguments"
          (mcodec/register! 0 :foo)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"Codec cbor is already registered with code 81"
          (mcodec/register! :cbor 0x1234)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                          #"Code 81 is already registered by codec cbor"
          (mcodec/register! :foo 0x51))))
  (testing "unregistration"
    (is (nil? (mcodec/unregister! :baz)))
    (is (nil? (mcodec/unregister! :baz))
        "repeat unregistration should succeed")
    (is (nil? (mcodec/key->code :baz)))
    (is (nil? (mcodec/register! :baz 0x1234)))))


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
