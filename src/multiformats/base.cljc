(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:refer-clojure :exclude [bases format])
  (:require
    [alphabase.core :as abc]
    [clojure.string :as str])
  #?(:clj
     (:import
       java.util.Base64)))


(def codes
  "Map of base keys to multicodec packed symbols from the standard table."
  {:base1         \1   ; Unary
   :base2         \0   ; Binary
   :base8         \7   ; Octal
   :base10        \9   ; Decimal
   :base16        \f   ; Hexadecimal (lower-case)
   :BASE16        \F   ; Hexadecimal (upper-case)
   :base32        \b   ; RFC 4648 (lower-case)
   :BASE32        \B   ; RFC 4648 (upper-case)
   :base32pad     \c   ; RFC 4648 (lower-case)
   :BASE32PAD     \C   ; RFC 4648 (upper-case)
   :base32hex     \v   ; RFC 4648 (lower-case)
   :BASE32HEX     \V   ; RFC 4648 (upper-case)
   :base32hexpad  \t   ; RFC 4648 (lower-case)
   :BASE32HEXPAD  \T   ; RFC 4648 (upper-case)
   :base58flickr  \Z   ; Base58 Flicker
   :base58btc     \z   ; Base58 Bitcoin
   :base64        \m   ; RFC 4648
   :base64pad     \M   ; RFC 4648
   :base64url     \u   ; RFC 4648
   :base64urlpad  \U   ; RFC 4648
   ,,,})



;; ## Base Encodings

;; ### Numeric Bases

; TODO: octal, decimal?

(def ^:private base16
  {:key :base16
   :alphabet "0123456789abcdef"
   :case-insensitive true})


;; ### Base32 - RFC 4648

(def ^:private base32
  {:key :base32
   :alphabet "abcdefghijklmnopqrstuvwxyz234567"
   :case-insensitive true})


(def ^:private base32hex
  {:key :base32hex
   :alphabet "0123456789abcdefghijklmnopqrstuv"
   :case-insensitive true})


;; ### Base32 - RFC 4648

(def ^:private base58btc
  {:key :base58btc
   :alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"})


;; ### Base64 - RFC 4648

; TODO: base64 / base64pad

(def ^:private base64url
  {:key :base64url
   :formatter (fn formatter
                [data]
                #?(:clj
                   (-> (Base64/getUrlEncoder)
                       (.withoutPadding)
                       (.encodeToString data))))
   :parser (fn parser
             [string]
             #?(:clj
                (.decode (Base64/getUrlDecoder) string)))})


(def ^:private base64urlpad
  {:key :base64urlpad
   :formatter (fn formatter
                [data]
                #?(:clj
                   (.encodeToString (Base64/getUrlEncoder) data)))
   :parser (fn parser
             [string]
             #?(:clj
                (.decode (Base64/getUrlDecoder) string)))})



;; ## Lookup Functions

(defn- lower-case?
  "Test if the given string is all lower-case."
  [s]
  (= s (str/lower-case s)))


(defn- base-formatter
  "Construct an encoding function from base parameters."
  [params]
  (or (:formatter params)
      (when-let [alphabet (:alphabet params)]
        (fn format-alphabet
          [data]
          (abc/encode alphabet data)))
      (throw (ex-info (str "Base " (:key params)
                           " does not specify an alphabet "
                           " or a formatter function.")
                      {:base (:key params)}))))


(defn- base-parser
  "Construct a decoding function from base params."
  [params]
  (or (:parser params)
      (when-let [alphabet (:alphabet params)]
        (cond
          (not (:case-insensitive params))
          (fn parse-alphabet
            [string]
            (abc/decode alphabet string))

          (lower-case? alphabet)
          (fn parse-lower
            [string]
            (abc/decode alphabet (str/lower-case string)))

          :else
          (fn parse-upper
            [string]
            (abc/decode alphabet (str/upper-case string)))))
      (throw (ex-info (str "Base " (:key params)
                           " does not specify an alphabet "
                           " or a parser function.")
                      {:base (:key params)}))))


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
                      {:base base-key})))
    (when-not prefix
      (throw (ex-info (str "Base " base-key " has no assigned prefix code!")
                      {:base base-key})))
    (when (contains? base-map base-key)
      (throw (ex-info (str "Base " base-key " is already registered!")
                      {:base base-key})))
    (when-let [extant (ffirst (filter (comp #{prefix} :prefix val) base-map))]
      (throw (ex-info (str "Prefix " (pr-str prefix)
                           " is already registered to " extant)
                      {:base base-key
                       :prefix prefix})))
    (let [definition (-> params
                         (dissoc :key)
                         (assoc :prefix prefix
                                :formatter (base-formatter params)
                                :parser (base-parser params)))
          base-map (assoc base-map base-key definition)]
      (if (and (:case-insensitive params)
               (lower-case? prefix))
        (recur base-map
               (assoc params
                      :key (keyword (str/upper-case (name base-key)))
                      :alphabet (str/upper-case (:alphabet params))))
        base-map))))


(def bases
  "Map of base keys to definition maps."
  (reduce install-base
          {}
          [base16
           base32
           base32hex
           base58btc
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
