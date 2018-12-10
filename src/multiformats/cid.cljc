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
  (let [header (b/byte-array 16)
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
    (if (= this that)
      0
      (let [[version codec _] (read-header _bytes)
            vc (compare version (:version that))]
        (if (zero? vc)
          (let [mcc (compare codec (:code that))]
            (if (zero? mcc)
              (compare (:hash this) (:hash that))
              mcc))
          vc))))


  ILookup

  (#?(:clj valAt, :cljs -lookup)
    [this k]
    (#?(:clj .valAt, :cljs -lookup) this k nil))


  (#?(:clj valAt, :cljs -lookup)
    [this k not-found]
    (let [[version codec hlength] (read-header _bytes)]
      (case k
        :length (alength _bytes)
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
  (let [codec (mcodec/resolve-code codec)]
    (when-not (instance? Multihash mhash)
      (throw (ex-info (str "Cannot construct CID with non-multihash value: "
                           (pr-str mhash))
                      {:codec codec
                       :hash mhash})))
    (let [version 1
          encoded (encode-bytes version codec mhash)]
      (->ContentID encoded nil 0))))
