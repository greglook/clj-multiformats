(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:refer-clojure :exclude [format])
  (:require
    [alphabase.core :as abc]
    [clojure.string :as str])
  ; TODO: cljc
  (:import
    java.util.Base64))


;; ## Base Mappings

; TODO: this won't really work in cljs

(def encodings
  "Set of supported multibase encoding keys."
  #{})


(def ^:private prefix->base
  "Cached map of prefix characters to base keys."
  {})


(def ^:private base->prefix
  "Cached map of base keys to prefix characters."
  {})


(def ^:private base->formatter
  "Cached map of base keys to encoding functions."
  {})


(def ^:private base->parser
  "Cached map of base keys to decoding functions."
  {})


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
          (not (:case-insensitive? params))
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


(defn register!
  "Register a new multibase encoding."
  [params]
  (let [base-key (:key params)
        prefix (:prefix params)]
    (when-not (and (keyword? base-key) (string? prefix))
      (throw (ex-info (str "Base registered with invalid key or prefix: "
                           (pr-str base-key) " / " (pr-str prefix))
                      {:base base-key
                       :prefix prefix})))
    (when (contains? encodings base-key)
      (throw (ex-info (str "Base " base-key " is already registered!")
                      {:base base-key})))
    (when-let [extant (prefix->base prefix)]
      (throw (ex-info (str "Prefix " (pr-str prefix)
                           " is already registered to " extant)
                      {:base base-key
                       :prefix prefix})))
    (let [formatter (base-formatter params)
          parser (base-parser params)]
      (alter-var-root #'encodings conj base-key)
      (alter-var-root #'prefix->base assoc prefix base-key)
      (alter-var-root #'base->prefix assoc base-key prefix)
      (alter-var-root #'base->formatter assoc base-key formatter)
      (alter-var-root #'base->parser assoc base-key parser))
    (when (and (:case-insensitive? params)
               (lower-case? prefix))
      (recur (assoc params
                    :key (keyword (str/upper-case (name base-key)))
                    :prefix (str/upper-case prefix)
                    :alphabet (str/upper-case (:alphabet params)))))))



;; ## Base Encodings

(register!
  {:key :base16
   :prefix "f"
   :alphabet "0123456789abcdef"
   :case-insensitive? true})


(register!
  {:key :base32
   :prefix "b"
   :alphabet "abcdefghijklmnopqrstuvwxyz234567"
   :case-insensitive? true})


(register!
  {:key :base32hex
   :prefix "v"
   :alphabet "0123456789abcdefghijklmnopqrstuv"
   :case-insensitive? true})


(register!
  {:key :base58btc
   :prefix "z"
   :alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"})


(register!
  {:key :base64
   :prefix "?" ; FIXME: real prefix
   :formatter (fn base64-format
                [data]
                (-> (Base64/getUrlEncoder)
                    (.withoutPadding)
                    (.encodeToString data)))
   :parser (fn base64-parse
             [string]
             (.decode (Base64/getUrlDecoder) string))})



;; ## Public API

(defn format*
  "Formats binary data into a string with the given base. Returns the formatted
  string without a prefix."
  ^String
  [base-key ^bytes data]
  (when (zero? (count data))
    (throw (ex-info "Cannot format empty data as a multibase string"
                    {:base base-key})))
  (when-not (contains? encodings base-key)
    (throw (ex-info (str (pr-str base-key)
                         " is not a supported multibase encoding")
                    {:base base-key})))
  (let [formatter (base->formatter base-key)]
    (formatter data)))


(defn format
  "Formats binary data into a string with the given base. Returns the formatted
  string, prefixed with the base constant."
  ^String
  [base-key ^bytes data]
  (let [prefix (base->prefix base-key)]
    (str prefix (format* base-key data))))


(defn- get-prefix
  "Get the multibase prefix character from the given string. Throws an
  exception if the string is too short."
  [^String string]
  (when (< (count string) 2)
    (throw (ex-info (str "The string " (pr-str string)
                         " is too short to be multibase-formatted data")
                    {:data string})))
  (str (first string)))


(defn parse
  "Parses a multibase-prefixed string into binary data. Returns an array with
  the parsed bytes, or throws an error if there is no known base."
  ^bytes
  [^String string]
  (let [prefix (get-prefix string)
        base-key (prefix->base prefix)
        parser (base->parser base-key)]
    (when-not base-key
      (throw (ex-info (str "The prefix " (pr-str prefix)
                           " does not map to a supported multibase encoding")
                      {:data string
                       :prefix prefix})))
    (let [parser (base->parser base-key)]
      (parser (subs string 1)))))


(defn inspect
  "Inspect a string and return a map of information including the prefix
  character, base key, and parsed data."
  [^String string]
  (let [prefix (get-prefix string)]
    {:prefix prefix
     :base (prefix->base prefix)
     :data (parse string)}))
