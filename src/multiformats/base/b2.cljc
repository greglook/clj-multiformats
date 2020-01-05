(ns multiformats.base.b2
  "Binary base encoding implementation."
  (:refer-clojure :exclude [format])
  (:require
    #?(:cljs [alphabase.bytes :as b])
    [clojure.string :as str])
  #?(:clj
     (:import
       (org.apache.commons.codec.binary
         BinaryCodec))))


#?(:clj
   (defn- reverse-octets
     "Reverse the octets (8-bit segments) a binary string."
     [string]
     (loop [octets nil
            i 0]
       (if (< i (count string))
         (recur (cons (subs string i (+ i 8)) octets) (+ i 8))
         (apply str octets)))))


#?(:cljs
   (defn- byte->octet
     "Convert a byte value to a binary octet string."
     [x]
     (str
       (if (zero? (bit-and x 0x80)) "0" "1")
       (if (zero? (bit-and x 0x40)) "0" "1")
       (if (zero? (bit-and x 0x20)) "0" "1")
       (if (zero? (bit-and x 0x10)) "0" "1")
       (if (zero? (bit-and x 0x08)) "0" "1")
       (if (zero? (bit-and x 0x04)) "0" "1")
       (if (zero? (bit-and x 0x02)) "0" "1")
       (if (zero? (bit-and x 0x01)) "0" "1"))))


#?(:cljs
   (defn- octet->byte
     "Convert a binary octet string to a byte value."
     [octet]
     (bit-or
       (if (= "1" (subs octet 0 1)) 0x80 0x00)
       (if (= "1" (subs octet 1 2)) 0x40 0x00)
       (if (= "1" (subs octet 2 3)) 0x20 0x00)
       (if (= "1" (subs octet 3 4)) 0x10 0x00)
       (if (= "1" (subs octet 4 5)) 0x08 0x00)
       (if (= "1" (subs octet 5 6)) 0x04 0x00)
       (if (= "1" (subs octet 6 7)) 0x02 0x00)
       (if (= "1" (subs octet 7 8)) 0x01 0x00))))


(defn format
  "Format some byte data into a binary-encoded string."
  [^bytes data]
  #?(:clj
     (reverse-octets (BinaryCodec/toAsciiString data))
     :cljs
     (reduce
       (fn build-str
         [string i]
         (str string (byte->octet (b/get-byte data i))))
       "" (range (alength data)))))


(defn- zero-pad
  "Pad the given binary string with zeroes so its length is a multiple of 8."
  [string]
  (if (zero? (rem (count string) 8))
    string
    (str (str/join (repeat (- 8 (rem (count string) 8)) "0"))
         string)))


(defn parse
  "Parse a string of binary-encoded bytes."
  [^String string]
  #?(:clj
     (let [digits (zero-pad string)]
       (.toByteArray (BinaryCodec.) (reverse-octets digits)))
     :cljs
     (let [digits (zero-pad string)
           buffer (b/byte-array (int (/ (count digits) 8)))]
       (dotimes [i (alength buffer)]
         (let [octet (subs digits (* i 8) (* (inc i) 8))]
           (b/set-byte buffer i (octet->byte octet))))
       buffer)))
