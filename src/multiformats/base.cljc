(ns multiformats.base
  "Multibase is a protocol for distinguishing base encodings and other simple
  string encodings, and for ensuring full compatibility with program
  interfaces.

  https://github.com/multiformats/multibase"
  (:require
    [alphabase.core :as abc]))


(def encodings
  "Set of supported multibase encodings."
  #{:base16
    :base32
    :base32hex
    :base58btc})


(defn- base->prefix
  "Returns the prefix character to use for a given base, or nil if the base is
  not recognized."
  [base-key]
  (case base-key
    ; TODO: support all-caps keywords as a shorthand for the case-insensitive ones?
    :base16    \f
    :base32    \b
    :base32hex \v
    :base58btc \z
    nil))


(defn- prefix->base
  "Resolve a leading varint prefix value to a multibase key. Returns nil if no
  matching base is found."
  [prefix]
  (case prefix
    (\f \F) :base16
    (\b \B) :base32
    (\v \V) :base32hex
    \z      :base58btc
    nil))


(defn- prefix->alphabet
  "Resolve a leading varint prefix value to a multibase alphabet. Returns nil
  if no matching base is found."
  [prefix]
  (case prefix
    \f "0123456789abcdef"
    \F "0123456789ABCDEF"
    \b "abcdefghijklmnopqrstuvwxyz234567"
    \B "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    \v "0123456789abcdefghijklmnopqrstuv"
    \V "0123456789ABCDEFGHIJKLMNOPQRSTUV"
    \z "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    nil))


(defn encode
  "Encodes binary data with the given base. Returns the encoded string,
  prefixed with the base constant."
  ^String
  [base-key ^bytes data]
  (when (zero? (count data))
    (throw (ex-info "Cannot encode empty data as a multibase string"
                    {:base base-key})))
  (let [prefix (base->prefix base-key)
        alphabet (prefix->alphabet prefix)]
    (when-not (and prefix alphabet)
      (throw (ex-info (str (pr-str base-key)
                           " is not a supported multibase encoding")
                      {:base base-key})))
    (str prefix (abc/encode alphabet data))))


(defn- get-prefix
  "Get the multibase prefix character from the given string. Throws an
  exception if the string is too short."
  [^String string]
  (when (< (count string) 2)
    (throw (ex-info (str "The string " (pr-str string)
                         " is too short to be multibase-encoded data")
                    {:data string})))
  (.charAt string 0))


(defn decode
  "Decodes a multibase-prefixed string into binary data. Returns an array with
  the decoded bytes, or throws an error if there is no known base."
  ^bytes
  [^String string]
  (let [prefix (get-prefix string)
        alphabet (prefix->alphabet prefix)]
    (when-not alphabet
      (throw (ex-info (str "The prefix " (pr-str prefix)
                           " does not map to a supported multibase encoding")
                      {:data string
                       :prefix prefix})))
    (abc/decode alphabet (subs string 1))))


(defn inspect
  "Inspect a string and return a map of information including the prefix
  character, base key, and decoded data."
  [^String string]
  (let [prefix (get-prefix string)]
    {:prefix prefix
     :base (prefix->base prefix)
     :data (decode string)}))
