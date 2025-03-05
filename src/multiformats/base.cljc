(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:refer-clojure :exclude [bases format])
  (:require
    [alphabase.base16 :as b16]
    [alphabase.base2 :as b2]
    [alphabase.base32 :as b32]
    [alphabase.base58 :as b58]
    [alphabase.base64 :as b64]
    [alphabase.base8 :as b8]
    [clojure.string :as str]))


(def codes
  "Map of base keys to multicodec packed symbols from the standard table."
  {;; Numeric
   :base1         \1
   :base2         \0
   :base8         \7
   :base10        \9
   ;; Hexadecimal (RFC 4648)
   :base16        \f
   :BASE16        \F
   ;; Base32 (RFC 4648)
   :base32        \b
   :BASE32        \B
   :base32pad     \c
   :BASE32PAD     \C
   :base32hex     \v
   :BASE32HEX     \V
   :base32hexpad  \t
   :BASE32HEXPAD  \T
   ;; Base58
   :base58btc     \z
   :base58flickr  \Z
   ;; Base64 (RFC 4648)
   :base64        \m
   :base64pad     \M
   :base64url     \u
   :base64urlpad  \U})


;; ## Base Encodings

(def ^:private binary-encodings
  "Binary (b2) and Octal (b8) encodings."
  [{:key :base2
    :formatter b2/encode
    :parser b2/decode}
   {:key :base8
    :formatter b8/encode
    :parser b8/decode}])


(def ^:private hex-encodings
  "Hexadecimal (b16) encodings."
  [{:key :base16
    :formatter (comp str/lower-case b16/encode)
    :parser b16/decode}
   {:key :BASE16
    :formatter b16/encode
    :parser b16/decode}])


(def ^:private b32-encodings
  "Base32 encodings."
  (letfn [(parse-base32
            [string]
            (b32/decode string false))

          (parse-base32hex
            [string]
            (b32/decode string true))]
    [{:key :base32
      :formatter (fn format-base32
                   [data]
                   (str/lower-case (b32/encode data false false)))
      :parser parse-base32}
     {:key :BASE32
      :formatter (fn format-BASE32
                   [data]
                   (b32/encode data false false))
      :parser parse-base32}
     {:key :base32pad
      :formatter (fn format-base32pad
                   [data]
                   (str/lower-case (b32/encode data false true)))
      :parser parse-base32}
     {:key :BASE32PAD
      :formatter (fn format-BASE32PAD
                   [data]
                   (b32/encode data false true))
      :parser parse-base32}
     {:key :base32hex
      :formatter (fn format-base32hex
                   [data]
                   (str/lower-case (b32/encode data true false)))
      :parser parse-base32hex}
     {:key :BASE32HEX
      :formatter (fn format-BASE32HEX
                   [data]
                   (b32/encode data true false))
      :parser parse-base32hex}
     {:key :base32hexpad
      :formatter (fn format-base32hexpad
                   [data]
                   (str/lower-case (b32/encode data true true)))
      :parser parse-base32hex}
     {:key :BASE32HEXPAD
      :formatter (fn format-BASE32HEXPAD
                   [data]
                   (b32/encode data true true))
      :parser parse-base32hex}]))


(def ^:private b58-encodings
  "Base58 encodings."
  [{:key :base58btc
    :formatter b58/encode
    :parser b58/decode}])


(def ^:private b64-encodings
  "Base64 encodings."
  [{:key :base64
    :formatter (fn format-base64
                 [data]
                 (b64/encode data false false))
    :parser b64/decode}
   {:key :base64pad
    :formatter (fn format-base64pad
                 [data]
                 (b64/encode data false true))
    :parser b64/decode}
   {:key :base64url
    :formatter (fn format-base64url
                 [data]
                 (b64/encode data true false))
    :parser b64/decode}
   {:key :base64urlpad
    :formatter (fn format-base64urlpad
                 [data]
                 (b64/encode data true true))
    :parser b64/decode}])


(def bases
  "Map of base keys to definition maps."
  (into {}
        (comp
          cat
          (map (fn prep-base
                 [params]
                 (let [base-key (:key params)
                       prefix (some-> (get codes base-key) str)]
                   [base-key (assoc params :prefix prefix)]))))
        [binary-encodings
         hex-encodings
         b32-encodings
         b58-encodings
         b64-encodings]))


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
  character, base key, and encoded data length."
  [^String string]
  (let [prefix (get-prefix string)
        base (prefix->base prefix)]
    {:base base
     :prefix prefix
     :length (dec (count string))}))
