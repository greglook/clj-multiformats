(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:refer-clojure :exclude [bases format])
  #?(:cljs
     (:require-macros
       [multiformats.base :refer [defbase]]))
  (:require
    [alphabase.bytes :as b]
    [alphabase.core :as abc]
    [clojure.string :as str]
    #?@(:cljs
        [[goog.crypt :as crypt]
         [goog.crypt.base64 :as gb64]]))
  #?(:clj
     (:import
       (org.apache.commons.codec.binary
         Base32
         Base64
         BinaryCodec
         Hex))))


(def codes
  "Map of base keys to multicodec packed symbols from the standard table."
  {; Numeric
   :base1         \1
   :base2         \0
   :base8         \7
   :base10        \9
   ; Hexadecimal (RFC 4648)
   :base16        \f
   :BASE16        \F
   ; Base32 (RFC 4648)
   :base32        \b
   :BASE32        \B
   :base32pad     \c
   :BASE32PAD     \C
   :base32hex     \v
   :BASE32HEX     \V
   :base32hexpad  \t
   :BASE32HEXPAD  \T
   ; Base58
   :base58btc     \z
   :base58flickr  \Z
   ; Base64 (RFC 4648)
   :base64        \m
   :base64pad     \M
   :base64url     \u
   :base64urlpad  \U})



;; ## Base Encodings

(defmacro ^:private defbase
  "Define a new base map in a var."
  [base-sym & {:as params}]
  `(def ~(vary-meta base-sym assoc :private true)
     (assoc ~params :key ~(keyword base-sym))))


;; ### Binary

(defn- reverse-octets
  "Reverse the octets (8-bit segments) a binary string."
  [string]
  (loop [octets nil
         i 0]
    (if (< i (count string))
      (recur (cons (subs string i (+ i 8)) octets) (+ i 8))
      (apply str octets))))


(defn- format-binary
  "Format some byte data into a binary-encoded string."
  [^bytes data]
  #?(:clj
     (reverse-octets (BinaryCodec/toAsciiString data))
     :default ; OPTIMIZE: better cljs implementation
     (reduce
       (fn build-str
         [string i]
         (let [x (b/get-byte data i)
               octet (str (if (zero? (bit-and x 0x80)) "0" "1")
                          (if (zero? (bit-and x 0x40)) "0" "1")
                          (if (zero? (bit-and x 0x20)) "0" "1")
                          (if (zero? (bit-and x 0x10)) "0" "1")
                          (if (zero? (bit-and x 0x08)) "0" "1")
                          (if (zero? (bit-and x 0x04)) "0" "1")
                          (if (zero? (bit-and x 0x02)) "0" "1")
                          (if (zero? (bit-and x 0x01)) "0" "1"))]
           (str string octet)))
       "" (range (alength data)))))


(defn- parse-binary
  "Parse a string of binary-encoded bytes."
  [^String string]
  #?(:clj
     (.toByteArray (BinaryCodec.) (reverse-octets string))
     :default ; OPTIMIZE: better cljs implementation
     (let [string (if (zero? (rem (count string) 8))
                    string
                    (str (str/join (repeat (- 8 (rem (count string) 8)) "0"))
                         string))
           buffer (b/byte-array (int (/ (count string) 8)))]
       (dotimes [i (alength buffer)]
         (let [octet (subs string (* i 8) (* (inc i) 8))
               x (bit-or (if (= "1" (subs octet 0 1)) 0x80 0x00)
                         (if (= "1" (subs octet 1 2)) 0x40 0x00)
                         (if (= "1" (subs octet 2 3)) 0x20 0x00)
                         (if (= "1" (subs octet 3 4)) 0x10 0x00)
                         (if (= "1" (subs octet 4 5)) 0x08 0x00)
                         (if (= "1" (subs octet 5 6)) 0x04 0x00)
                         (if (= "1" (subs octet 6 7)) 0x02 0x00)
                         (if (= "1" (subs octet 7 8)) 0x01 0x00))]
           (b/set-byte buffer i x)))
       buffer)))


(defbase base2
  :formatter format-binary
  :parser parse-binary)


;; ### Octal

(let [alphabet "01234567"]
  (defbase base8
    :formatter (fn format-octal
                 [^bytes data]
                 (abc/encode alphabet data))
    :parser (fn parse-octal
              [^String string]
              (abc/decode alphabet string))))


;; ### Hexadecimal

(defn- format-hex
  "Format byte data as a hexadecimal-encoded string."
  [^bytes data]
  #?(:clj
     (Hex/encodeHexString data true)
     :cljs
     (crypt/byteArrayToHex data)))


(defn- parse-hex
  "Parse a hexadecimal-encoded string into bytes."
  [^String string]
  #?(:clj
     (Hex/decodeHex string)
     :cljs
     (crypt/hexToByteArray string)))


(defbase base16
  :formatter format-hex
  :parser parse-hex)


(defbase BASE16
  :formatter (comp str/upper-case format-hex)
  :parser parse-hex)


;; ### Base32 (RFC 4648)

(defn- base32-formatter
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


(defn- base32-parser
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


(defbase base32
  :formatter (base32-formatter false true false)
  :parser (base32-parser false true false))


(defbase BASE32
  :formatter (base32-formatter false false false)
  :parser (base32-parser false false false))


(defbase base32pad
  :formatter (base32-formatter false true true)
  :parser (base32-parser false true true))


(defbase BASE32PAD
  :formatter (base32-formatter false false true)
  :parser (base32-parser false false true))


(defbase base32hex
  :formatter (base32-formatter true true false)
  :parser (base32-parser true true false))


(defbase BASE32HEX
  :formatter (base32-formatter true false false)
  :parser (base32-parser true false false))


(defbase base32hexpad
  :formatter (base32-formatter true true true)
  :parser (base32-parser true true true))


(defbase BASE32HEXPAD
  :formatter (base32-formatter true false true)
  :parser (base32-parser true false true))



;; ### Base58

(let [alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"]
  (defbase base58btc
    :formatter (fn format-base58btc
                 [^bytes data]
                 (abc/encode alphabet data))
    :parser (fn parse-base58btc
              [^String string]
              (abc/decode alphabet string))))


;; ### Base64 (RFC 4648)

(defn- base64-formatter
  "Construct a new function to format bytes as base64 with normal or URL
  alphabet, padded or not."
  [url? padding?]
  (fn format
    [^bytes data]
    #?(:clj
       (let [encoded (if url?
                       (Base64/encodeBase64URLSafeString data)
                       (Base64/encodeBase64String data))]
         (cond
           (and url? padding?)
           (str encoded (case (rem (alength data) 3)
                          2 "="
                          1 "=="
                          nil))

           (and (not url?) (not padding?))
           (str/replace encoded #"=+$" "")

           :else encoded))
       :cljs
       (let [encoded (gb64/encodeByteArray data url?)]
         (if padding?
           (str/replace encoded "." "=")
           (str/replace encoded #"[.=]+$" ""))))))


(defn- parse-base64
  "Parse a string of base64-encoded bytes."
  [^String string]
  #?(:clj
     (Base64/decodeBase64 string)
     :cljs
     (gb64/decodeStringToUint8Array string)))


(defbase base64
  :formatter (base64-formatter false false)
  :parser parse-base64)


(defbase base64pad
  :formatter (base64-formatter false true)
  :parser parse-base64)


(defbase base64url
  :formatter (base64-formatter true false)
  :parser parse-base64)


(defbase base64urlpad
  :formatter (base64-formatter true true)
  :parser parse-base64)



;; ## Lookup Functions

(defn- install-base
  "Expands a base definition map into one or more definitions and adds them to
  the given map of bases. Returns the updated map, or throws an exception if
  there is a conflict or insufficient information."
  [base-map params]
  (let [base-key (:key params)
        prefix (some-> (get codes base-key) str)]
    (when-not (keyword? base-key)
      (throw (ex-info (str "Base registered with invalid key: "
                           (pr-str base-key))
                      {:params params})))
    (when-not prefix
      (throw (ex-info (str "Base " base-key " has no assigned prefix code!")
                      {:params params})))
    (when (contains? base-map base-key)
      (throw (ex-info (str "Base " base-key " is already registered!")
                      {:params params})))
    (when-not (:formatter params)
      (throw (ex-info (str "Base " base-key " does not specify a formatter function.")
                      {:params params})))
    (when-not (:parser params)
      (throw (ex-info (str "Base " base-key " does not specify a parser function.")
                      {:params params})))
    (assoc base-map base-key (assoc params :prefix prefix))))


(def bases
  "Map of base keys to definition maps."
  (reduce install-base
          {}
          [base2
           base8
           base16
           BASE16
           base32
           BASE32
           base32pad
           BASE32PAD
           base32hex
           BASE32HEX
           base32hexpad
           BASE32HEXPAD
           base58btc
           base64
           base64pad
           base64url
           base64urlpad]))


(def ^:private prefix->base
  "Cached map of prefix characters to base keys."
  (into {} (map (juxt (comp :prefix val) key)) bases))



;; ## Encoding

(defn format*
  "Formats binary data into a string with the given base. Returns the formatted
  string without a prefix."
  ^String
  [base-key ^bytes data]
  (when-not (keyword? base-key)
    (throw (ex-info "base-key must be a keyword" {:base base-key})))
  (when (zero? (alength data))
    (throw (ex-info "Cannot format empty data as a multibase string"
                    {:base base-key})))
  (let [formatter (get-in bases [base-key :formatter])]
    (when-not formatter
      (throw (ex-info (str (name base-key) " does not have a supported multibase formatter")
                      {:base base-key})))
    (formatter data)))


(defn format
  "Formats binary data into a string with the given base. Returns the formatted
  string, prefixed with the base constant."
  ^String
  [base-key ^bytes data]
  (let [prefix (get codes base-key)]
    (str prefix (format* base-key data))))



;; ## Decoding

(defn- get-prefix
  "Get the multibase prefix character from the given string. Throws an
  exception if the string is too short."
  [^String string]
  (when (< (count string) 2)
    (throw (ex-info (str "The string " (pr-str string)
                         " is too short to be multibase-formatted data")
                    {:data string})))
  (str (first string)))


(defn parse*
  "Parse a non-prefixed string into binary data using the given base. Returns
  the decoded byte array."
  ^bytes
  [base-key string]
  (when-not (keyword? base-key)
    (throw (ex-info "base-key must be a keyword" {:base base-key})))
  (let [parser (get-in bases [base-key :parser])]
    (when-not parser
      (throw (ex-info (str (name base-key) " does not have a supported multibase parser")
                      {:base base-key})))
    (parser string)))


(defn parse
  "Parses a multibase-prefixed string into binary data. Returns an array with
  the parsed bytes, or throws an error if there is no known base."
  ^bytes
  [^String string]
  (let [prefix (get-prefix string)
        base-key (prefix->base prefix)]
    (when-not base-key
      (throw (ex-info (str "The prefix " (pr-str prefix)
                           " does not map to a supported multibase encoding")
                      {:data string
                       :prefix prefix})))
    (parse* base-key (subs string 1))))


(defn inspect
  "Inspect a string and return a map of information including the prefix
  character, base key, and parsed data."
  [^String string]
  (let [prefix (get-prefix string)]
    {:prefix prefix
     :base (prefix->base prefix)
     :data (parse string)}))
