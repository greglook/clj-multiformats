(ns multiformats.base.b64
  "Base64 implementation from RFC 4648."
  (:require
    #?(:clj [clojure.string :as str]
       :cljs [goog.crypt.base64 :as gcb64]))
  #?(:clj
     (:import
       (org.apache.commons.codec.binary
         Base64))))


(defn formatter
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
       (gcb64/encodeByteArray
         data
         (if-not url?
           (if padding?
             (.-DEFAULT gcb64/Alphabet)
             (.-NO_PADDING gcb64/Alphabet))
           (if padding?
             (.-WEBSAFE gcb64/Alphabet)
             (.-WEBSAFE_NO_PADDING gcb64/Alphabet)))))))


(defn parse
  "Parse a string of base64-encoded bytes."
  [^String string]
  #?(:clj
     (Base64/decodeBase64 string)
     :cljs
     (gcb64/decodeStringToUint8Array string)))
