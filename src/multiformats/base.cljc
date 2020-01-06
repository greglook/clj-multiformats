(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:refer-clojure :exclude [bases format])
  (:require
    [clojure.string :as str]
    [multiformats.base.b16 :as b16]
    [multiformats.base.b2 :as b2]
    [multiformats.base.b32 :as b32]
    [multiformats.base.b58 :as b58]
    [multiformats.base.b64 :as b64]
    [multiformats.base.b8 :as b8])
  #?(:cljs
     (:require-macros
       [multiformats.base :refer [defbase]])))


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


;; Appease clj-kondo
(declare base2
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
         base64urlpad)


;; ### Binary

(defbase base2
  :formatter b2/format
  :parser b2/parse)


;; ### Octal

(defbase base8
  :formatter b8/format
  :parser b8/parse)


;; ### Hexadecimal

(defbase base16
  :formatter b16/format
  :parser b16/parse)


(defbase BASE16
  :formatter (comp str/upper-case b16/format)
  :parser b16/parse)


;; ### Base32 (RFC 4648)

(defbase base32
  :formatter (b32/formatter false true false)
  :parser (b32/parser false))


(defbase BASE32
  :formatter (b32/formatter false false false)
  :parser (b32/parser false))


(defbase base32pad
  :formatter (b32/formatter false true true)
  :parser (b32/parser false))


(defbase BASE32PAD
  :formatter (b32/formatter false false true)
  :parser (b32/parser false))


(defbase base32hex
  :formatter (b32/formatter true true false)
  :parser (b32/parser true))


(defbase BASE32HEX
  :formatter (b32/formatter true false false)
  :parser (b32/parser true))


(defbase base32hexpad
  :formatter (b32/formatter true true true)
  :parser (b32/parser true))


(defbase BASE32HEXPAD
  :formatter (b32/formatter true false true)
  :parser (b32/parser true))


;; ### Base58

(defbase base58btc
  :formatter b58/format-btc
  :parser b58/parse-btc)


;; ### Base64 (RFC 4648)

(defbase base64
  :formatter (b64/formatter false false)
  :parser b64/parse)


(defbase base64pad
  :formatter (b64/formatter false true)
  :parser b64/parse)


(defbase base64url
  :formatter (b64/formatter true false)
  :parser b64/parse)


(defbase base64urlpad
  :formatter (b64/formatter true true)
  :parser b64/parse)



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
     :base (prefix->base prefix)}))
