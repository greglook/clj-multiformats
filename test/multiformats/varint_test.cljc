(ns multiformats.varint-test
  (:require
    [alphabase.bytes :refer [bytes= init-bytes]]
    [clojure.test :refer [deftest testing is]]
    [multiformats.varint :as varint]))


(deftest varint-coding
  (let [examples {    0 [0x00]
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
      (is (bytes= bs (varint/encode n)))
      (is (= n (varint/decode (init-bytes bs)))))))


(deftest edge-cases
  (testing "encoding"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cannot be negative"
          (varint/encode -1)))
    ; write varint larger than 9 bytes
    ; write beyond end of buffer
    ,,,)
  (testing "decoding"
    ; read varint larger than 9 bytes
    ; read beyond end of buffer
    ,,,))
