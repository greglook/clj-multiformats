(ns multiformats.varint-test
  (:require
    [alphabase.bytes :as b :refer [bytes=]]
    #?(:clj [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])
    [multiformats.varint :as varint]))


(deftest varint-coding
  (let [examples {0 [0x00]
                  1 [0x01]
                  127 [0x7F]
                  128 [0x80 0x01]
                  129 [0x81 0x01]
                  255 [0xFF 0x01]
                  256 [0x80 0x02]
                  257 [0x81 0x02]
                  300 [0xAC 0x02]
                  16384 [0x80 0x80 0x01]}]
    (doseq [[n bs] examples]
      (is (= bs (b/byte-seq (varint/encode n))))
      (is (= n (varint/decode (b/init-bytes bs)))))))


(deftest edge-cases
  (testing "encoding"
    (testing "negative value"
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                            #"cannot be negative"
            (varint/encode -1))))
    #_ ; TODO: doesn't work since bit shifts on a bigint fail
    (testing "varint larger than nine bytes"
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                            #"larger than nine bytes"
            (varint/encode 9223372036854775808))))
    (testing "out of buffer bounds"
      (let [buffer (b/byte-array 1)]
        (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                              #"write index out of bounds"
              (varint/write-bytes 255 buffer 0))))))
  (testing "decoding"
    (testing "varint larger than nine bytes"
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                            #"larger than nine bytes"
            (varint/decode (b/init-bytes [0x80 0x80 0x80 0x80 0x80
                                          0x80 0x80 0x80 0x80 0x01])))))
    (testing "out of buffer bounds"
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error)
                            #"out of bytes to decode"
            (varint/decode (b/init-bytes [0x80 0x81])))))))
