(ns multiformats.base.b16
  "Hexadecimal base encoding implementation."
  (:refer-clojure :exclude [format])
  #?(:cljs
     (:require
       [goog.crypt :as crypt]))
  #?(:clj
     (:import
       (org.apache.commons.codec.binary
         Hex))))


(defn format
  "Format byte data as a hexadecimal-encoded string."
  [^bytes data]
  #?(:clj
     (Hex/encodeHexString data true)
     :cljs
     (crypt/byteArrayToHex data)))


(defn parse
  "Parse a hexadecimal-encoded string into bytes."
  [^String string]
  #?(:clj
     (Hex/decodeHex string)
     :cljs
     (crypt/hexToByteArray string)))
