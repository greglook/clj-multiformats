(ns multiformats.base.b32
  "Base32 implementation from RFC 4648."
  (:require
   [alphabase.bytes :as b]
   [clojure.string :as str])
  #?(:clj
     (:import
      (org.apache.commons.codec.binary
       Base32))))

(defn formatter
  "Constructs a function which formats byte data as a base32-encoded string."
  [hex? lower? pad?]
  #?(:clj
     (let [codec (Base32. 0 nil hex? (int \=))]
       (fn format
         [^bytes data]
         (cond-> (.encodeToString codec data)
           lower? (str/lower-case)
           (not pad?) (str/replace #"=+$" ""))))
     :cljs
     (let [alphabet (cond-> (if hex?
                              "0123456789abcdefghijklmnopqrstuv"
                              "abcdefghijklmnopqrstuvwxyz234567")
                      (not lower?) (str/upper-case))]
       (fn format
         [data]
         (let [padding (rem (alength data) 5)]
           (loop [groups []
                  offset 0]
             (if (< offset (alength data))
               ; Read in 40 bits as 5 octets, write 8 characters.
               (let [input-bytes (min 5 (- (alength data) offset))
                     output-chars (cond-> (int (/ (* input-bytes 8) 5))
                                    (pos? (rem (* input-bytes 8) 5)) (inc))
                     [b0 b1 b2 b3 b4 b5] (map #(or (b/get-byte data (+ offset %)) 0)
                                              (range 0 input-bytes))
                     bits [; top 5 bits of byte 0
                           (bit-and (bit-shift-right b0 3) 0x1F)
                           ; bottom 3 bits of byte 0 + top 2 bits of byte 1
                           (bit-or (bit-and (bit-shift-left  b0 2) 0x1C)
                                   (bit-and (bit-shift-right b1 6) 0x03))
                           ; middle 5 bits of byte 1
                           (bit-and (bit-shift-right b1 1) 0x1F)
                           ; bottom 1 bit of byte 1 + top 4 bits of byte 2
                           (bit-or (bit-and (bit-shift-left  b1 4) 0x10)
                                   (bit-and (bit-shift-right b2 4) 0x0F))
                           ; bottom 4 bits of byte 2 + top 1 bit of byte 3
                           (bit-or (bit-and (bit-shift-left  b2 1) 0x1E)
                                   (bit-and (bit-shift-right b3 7) 0x01))
                           ; middle 5 bits of byte 3
                           (bit-and (bit-shift-right b3 2) 0x1F)
                           ; bottom 2 bits of byte 3 + top 3 bits of byte 4
                           (bit-or (bit-and (bit-shift-left  b3 3) 0x18)
                                   (bit-and (bit-shift-right b4 5) 0x07))
                           ; bottom 5 bits of byte 4
                           (bit-and b4 0x1F)]
                     s (apply str (take output-chars (map #(nth alphabet %) bits)))]
                 (recur (conj groups s) (+ offset 5)))
               ; Apply padding to final result.
               (cond-> (apply str groups)
                 pad? (str (case padding
                             4 "="
                             3 "==="
                             2 "===="
                             1 "======"
                             nil))))))))))

(defn parser
  "Constructs a function which parses a base32-encoded string into bytes."
  [hex? lower? pad?]
  #?(:clj
     (let [codec (Base32. 0 nil hex? (int \=))]
       (fn parse
         [^String string]
         (.decode codec string)))
     :cljs
     (let [alphabet (if hex?
                      "0123456789abcdefghijklmnopqrstuv"
                      "abcdefghijklmnopqrstuvwxyz234567")
           char->n (into {} (map vector (seq alphabet) (range)))]
       (fn parse
         [string]
         (let [input (str/replace (str/lower-case string) #"=+$" "")
               length (let [l (* 5 (int (/ (count input) 8)))]
                        (case (rem (count input) 8)
                          0 (+ l 0)
                          2 (+ l 1)
                          4 (+ l 2)
                          5 (+ l 3)
                          7 (+ l 4)))
               buffer (b/byte-array length)]
           (loop [char-offset 0
                  byte-offset 0]
             (when (< char-offset (count string))
               ; Read in 40 bits as 8 characters, write 5 octets.
               (let [input-chars (min 8 (- (count input) char-offset))
                     output-bytes (case input-chars
                                    2 1
                                    4 2
                                    5 3
                                    7 4
                                    8 5)
                     [c0 c1 c2 c3 c4 c5 c6 c7 :as cs]
                     (concat (map #(char->n (nth input (+ char-offset %)))
                                  (range input-chars))
                             (repeat (- 8 input-chars) 0))
                     [b0 b1 b2 b3 b4 b5 :as bs]
                     [; 5 bits of c0 + top 3 bits of c1
                      (bit-or (bit-and 0xF8 (bit-shift-left  c0 3))
                              (bit-and 0x07 (bit-shift-right c1 2)))
                      ; bottom 2 bits of c1 + 5 bits of c2 + top 1 bit of c3
                      (bit-or (bit-and 0xC0 (bit-shift-left  c1 6))
                              (bit-and 0x3E (bit-shift-left  c2 1))
                              (bit-and 0x01 (bit-shift-right c3 4)))
                      ; bottom 4 bits of c3 + top 4 bits of c4
                      (bit-or (bit-and 0xF0 (bit-shift-left  c3 4))
                              (bit-and 0x0F (bit-shift-right c4 1)))
                      ; bottom 1 bits of c4 + 5 bits of c5 + top 2 bit of c6
                      (bit-or (bit-and 0x80 (bit-shift-left  c4 7))
                              (bit-and 0x7C (bit-shift-left  c5 2))
                              (bit-and 0x03 (bit-shift-right c6 3)))
                      ; bottom 3 bits of c6 + 5 bits of c7
                      (bit-or (bit-and 0xE0 (bit-shift-left c6 5))
                              (bit-and 0x1F c7))]]
                 (dotimes [i output-bytes]
                   (b/set-byte buffer (+ byte-offset i) (nth bs i))))
               (recur (+ char-offset 8) (+ byte-offset 5))))
           buffer)))))
