(ns multiformats.base.b16
  "Hexadecimal base encoding implementation."
  (:refer-clojure :exclude [format])
  (:require
   [alphabase.bytes :as b]
   #?(:cljs [goog.crypt :as crypt]))
  #?(:clj
     (:import
      (org.apache.commons.codec.binary
       Hex))))

(defn byte->hex
  "Converts a single byte value to a two-character hex string."
  [value]
  (let [hex #?(:clj (Integer/toHexString value)
               :cljs (.toString value 16))]
    (if (= 1 (count hex))
      (str "0" hex)
      hex)))

(defn format-slice
  "Format a slice of a byte array as a hex string."
  [data offset length]
  (->> (range offset (+ offset length))
       (map #(byte->hex (b/get-byte data %)))
       (apply str)))

(defn format
  "Format byte data as a hexadecimal-encoded string."
  [^bytes data]
  #?(:clj (Hex/encodeHexString data true)
     :cljs (crypt/byteArrayToHex data)))

(defn parse
  "Parse a hexadecimal-encoded string into bytes."
  [^String string]
  #?(:clj (Hex/decodeHex string)
     :cljs (crypt/hexToByteArray string)))
