(ns multiformats.cid
  "CID is a self-describing content-addressed identifier. It uses cryptographic
  hashing to identify content, multicodec packed codes to label the content
  type, and multibase to encode the final identifier into a string.

  https://github.com/ipld/cid"
  (:refer-clojure :exclude [format])
  (:require
    [alphabase.bytes :as b]
    [clojure.string :as str]
    [multiformats.base :as mbase]
    [multiformats.base.b58 :as b58]
    [multiformats.codec :as mcodec]
    [multiformats.hash :as mhash]
    [multiformats.varint :as varint])
  #?(:clj
     (:import
       (clojure.lang
         ILookup
         IMeta
         IObj
         Keyword)
       multiformats.hash.Multihash)))


;; ## Coding Functions

(defn- read-header
  "Read the cid version and codec code from the encoded bytes. Returns a tuple of
  the numeric version, code, and number of bytes read."
  [^bytes data]
  (let [[version vsize] (varint/read-bytes data 0)
        [codec csize] (varint/read-bytes data vsize)]
    [version codec (+ vsize csize)]))


(defn- decode-parameters
  "Read the header and multihash from the encoded bytes to build a map of cid
  parameters. Resolves the codec to a keyword if possible."
  [^bytes data]
  (let [[version codec offset] (read-header data)
        [mhash hsize] (mhash/read-bytes data offset)]
    {:version version
     :codec (mcodec/resolve-key codec)
     :hash mhash}))


(defn- encode-bytes
  "Encode a cid version, codec, and multihash into a byte array."
  [version codec mhash]
  (let [header (b/byte-array 8)
        vlength (varint/write-bytes version header 0)
        clength (varint/write-bytes codec header vlength)
        hlength (+ vlength clength)
        buffer (b/byte-array (+ hlength (:length mhash)))]
    (b/copy header 0 buffer 0 hlength)
    (mhash/write-bytes mhash buffer hlength)
    buffer))



;; ## ContentID Type

(deftype ContentID
  [^bytes _bytes
   _meta
   ^:unsynchronized-mutable _hash]

  Object

  (toString
    [this]
    (let [params (decode-parameters _bytes)
          codec (:codec params)
          codec-name (if (keyword? codec)
                       (name codec)
                       (or codec "?"))
          algorithm (get-in params [:hash :algorithm])
          algo-name (if (keyword? algorithm)
                      (name algorithm)
                      (or algorithm "?"))]
      (str "cidv" (:version params)
           ":" codec-name ":" algo-name ":"
           (get-in params [:hash :digest] "?"))))


  #?(:clj java.io.Serializable)


  #?(:cljs IEquiv)

  (#?(:clj equals, :cljs -equiv)
    [this that]
    (cond
      (identical? this that) true

      (instance? ContentID that)
      (b/bytes= _bytes (#?(:clj ._bytes, :cljs .-_bytes) ^ContentID that))

      :else false))


  #?(:cljs IHash)

  (#?(:clj hashCode, :cljs -hash)
    [this]
    (let [hc _hash]
      (if (zero? hc)
        (let [params (decode-parameters _bytes)
              hc (hash [::cid (:version params) (:codec params) (:hash params)])]
          (set! _hash hc)
          hc)
        hc)))


  #?(:clj Comparable, :cljs IComparable)

  (#?(:clj compareTo, :cljs -compare)
    [this that]
    (cond
      (identical? this that) 0

      (instance? ContentID that)
      (b/compare _bytes (#?(:clj ._bytes :cljs .-_bytes) ^ContentID that))

      :else
      (throw (ex-info
               (str "Cannot compare CID value to " (type that))
               {:this this
                :that that}))))


  ILookup

  (#?(:clj valAt, :cljs -lookup)
    [this k]
    (#?(:clj .valAt, :cljs -lookup) this k nil))


  (#?(:clj valAt, :cljs -lookup)
    [this k not-found]
    (let [[version codec hlength] (read-header _bytes)]
      (case k
        :length (if (zero? version)
                  (- (alength _bytes) hlength)
                  (alength _bytes))
        :version version
        :codec (mcodec/resolve-key codec)
        :code codec
        :hash (first (mhash/read-bytes _bytes hlength))
        not-found)))


  IMeta

  (#?(:clj meta, :cljs -meta)
    [this]
    _meta)


  #?(:clj IObj, :cljs IWithMeta)

  (#?(:clj withMeta, :cljs -with-meta)
    [this meta-map]
    (ContentID. _bytes meta-map _hash)))


(alter-meta! #'->ContentID assoc :private true)



;; ## Construction

(defn create
  "Construct a new v1 content identifier for a known codec type and multihash."
  [codec mhash]
  (when-not (instance? multiformats.hash.Multihash mhash)
    (throw (ex-info (str "Cannot construct CID with non-multihash value: "
                         (pr-str mhash))
                    {:codec codec
                     :hash mhash})))
  (let [version 1
        code (mcodec/resolve-code codec)
        encoded (encode-bytes version code mhash)]
    (->ContentID encoded nil 0)))



;; ## Binary Serialization

(defn- inner-bytes
  "Retrieve the inner encoded bytes from a CID value."
  ^bytes
  [^ContentID cid]
  (#?(:clj ._bytes :cljs .-_bytes) cid))


(defn read-bytes
  "Read a content identifier from a byte array. Returns a tuple containing the
  CID and the number of bytes read."
  [^bytes data offset]
  (let [[version vlength] (varint/read-bytes data offset)]
    (when (not= 1 version)
      (throw (ex-info
               (str "Unable to decode CID version " (pr-str version))
               {:version version})))
    (let [[code clength] (varint/read-bytes data (+ offset vlength))
          [mhash hlength] (mhash/read-bytes data (+ offset vlength clength))
          length (+ vlength clength hlength)
          buffer (b/byte-array length)]
      (b/copy data offset buffer 0 length)
      [(->ContentID buffer nil 0) length])))


(defn write-bytes
  "Write an encoded content identifier to a byte array at the given offset.
  Returns the number of bytes written."
  [^ContentID cid ^bytes buffer offset]
  (let [encoded (inner-bytes cid)
        [version code hsize] (read-header encoded)]
    (if (zero? version)
      (b/copy encoded hsize buffer offset (- (alength encoded) hsize))
      (b/copy encoded buffer offset))))


(defn encode
  "Encode a content identifier into a binary representation. Returns the byte
  array."
  ^bytes
  [^ContentID cid]
  (if (zero? (:version cid))
    (mhash/encode (:hash cid))
    (b/copy (inner-bytes cid))))


(defn decode
  "Decode a content identifier from a byte array."
  [^bytes data]
  (if (and (= 34 (alength data))
           (= 0x12 (b/get-byte data 0))
           (= 0x20 (b/get-byte data 1)))
    ; v0 CID (bare multihash)
    (let [mhash (mhash/decode data)
          ; This is a bit of a departure from the IPLD/CID spec, but using the
          ; raw codec for 'unknown' seems generally more sensible.
          code (mcodec/resolve-code :raw)
          encoded (encode-bytes 0 code mhash)]
      (->ContentID encoded nil 0))
    ; v1+ CID
    (first (read-bytes data 0))))



;; ## String Serialization

(defn format
  "Format a CID into a string representation. Returns a multibase-prefixed
  string representing the encoded value. Uses base32 if not otherwise
  specified."
  ([cid]
   (if (zero? (:version cid))
     (mbase/format* :base58btc (mhash/encode (:hash cid)))
     (format :base32 cid)))
  ([base cid]
   (if (zero? (:version cid))
     (throw (ex-info "v0 CID values cannot be formatted in alternative bases"
                     {:cid cid}))
     (mbase/format base (inner-bytes cid)))))


(defn parse
  "Parse a content identifier from a base-encoded string."
  [string]
  (if (and (= 46 (count string)) (str/starts-with? string "Qm"))
    (decode (b58/parse-btc string))
    (decode (mbase/parse string))))
