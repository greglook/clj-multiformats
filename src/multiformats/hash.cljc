(ns multiformats.hash
  "Multihash is a protocol for differentiating outputs from various
  well-established cryptographic hash functions, addressing size and encoding
  considerations.

  https://github.com/multiformats/multihash"
  ;(:refer-clojure :exclude [format])
  (:require
    [alphabase.bytes :as b]
    [multiformats.base.b16 :as hex]
    [multiformats.varint :as varint])
  #?(:clj
     (:import
       (clojure.lang
         ILookup
         IMeta
         IObj
         Keyword))))


(def codes
  "Hash algorithm identifiers for use in multihashes."
  {:identity     0x00
   :md4          0xd4
   :md5          0xd5
   :sha1         0x11
   :sha2-256     0x12
   :sha2-512     0x13
   :dbl-sha2-256 0x56
   :sha3-224     0x17
   :sha3-256     0x16
   :sha3-384     0x15
   :sha3-512     0x14
   :shake-128    0x18
   :shake-256    0x19
   :keccak-224   0x1A
   :keccak-256   0x1B
   :keccak-384   0x1C
   :keccak-512   0x1D
   :murmur3      0x22
   :x11          0x1100})



;; ## Coding Functions

(defn- read-header
  "Read the algorithm code and digest bit size from the encoded bytes. Returns
  a tuple of the numeric code, byte length, and number of bytes read."
  [^bytes data]
  (let [[code csize] (varint/read-bytes data 0)
        [length lsize] (varint/read-bytes data csize)]
    [code length (+ csize lsize)]))


(defn- find-algorithm
  "Look up an algorithm key by code. Returns nil if no matching algorithm is
  present in the table."
  [code]
  (some #(when (= code (val %)) (key %)) codes))


(defn- decode-parameters
  "Read the header and digest from the encoded bytes."
  [^bytes data]
  (let [[code length offset] (read-header data)
        digest (hex/format-slice data offset (- (alength data) offset))]
    {:code code
     :algorithm (find-algorithm code)
     :length length
     :digest digest}))


(defn- encode-bytes
  "Encode a multihash algorithm, digest length, and digest bytes into a single
  byte array."
  [code ^bytes digest]
  (when (or (nil? digest) (zero? (alength digest)))
    (throw (ex-info "Cannot encode a multihash with an empty digest"
                    {:code code})))
  (let [code-bytes (varint/encode code)
        length-bytes (varint/encode (alength digest))
        buffer (b/byte-array (+ (alength code-bytes)
                                (alength length-bytes)
                                (alength digest)))]
    (b/copy code-bytes 0
            buffer 0
            (alength code-bytes))
    (b/copy length-bytes 0
            buffer (alength code-bytes)
            (alength length-bytes))
    (b/copy digest 0
            buffer (+ (alength code-bytes) (alength length-bytes))
            (alength digest))
    buffer))



;; ## Multihash Type

(deftype Multihash
  [^bytes _bytes
   _meta
   ^:unsynchronized-mutable _hash]

  Object

  (toString
    [this]
    (let [params (decode-parameters _bytes)
          algo (if-let [algorithm (:algorithm params)]
                 (name algorithm)
                 (:code params))]
      (str "hash:" algo \: (:digest params))))


  #?(:clj java.io.Serializable)

  ; TODO: customize serialization?


  #?(:cljs IEquiv)

  (#?(:clj equals, :cljs -equiv)
    [this that]
    (cond
      (identical? this that) true

      (instance? Multihash that)
      (b/bytes= _bytes (#?(:clj ._bytes :cljs .-_bytes) ^Multihash that))

      :else false))


  #?(:cljs IHash)

  (#?(:clj hashCode, :cljs -hash)
    [this]
    (let [hc _hash]
      (if (zero? hc)
        (let [params (decode-parameters _bytes)
              hc (int (-> (hash ::multihash)
                          (hash-combine (hash (:code params)))
                          (hash-combine (hash (:digest params)))))]
          (set! _hash hc)
          hc)
        hc)))


  #?(:clj Comparable, :cljs IComparable)

  (#?(:clj compareTo, :cljs -compare)
    [this that]
    ; OPTIMIZE: compare byte representations directly
    (cond
      (= this that) 0
      (neg? (compare (:algorithm this) (:algorithm that))) -1
      (pos? (compare (:algorithm this) (:algorithm that)))  1
      :else (compare (:digest this) (:digest that))))


  ILookup

  (#?(:clj valAt, :cljs -lookup)
    [this k]
    (#?(:clj .valAt, :cljs -lookup) this k nil))


  (#?(:clj valAt, :cljs -lookup)
    [this k not-found]
    (case k
      :code (first (read-header _bytes))
      :algorithm (let [[code] (read-header _bytes)]
                   (find-algorithm code))
      :bits (let [[_ length] (read-header _bytes)]
              (* length 8))
      :digest (:digest (decode-parameters _bytes))
      not-found))


  IMeta

  (#?(:clj meta, :cljs -meta)
    [this]
    _meta)


  #?(:clj IObj, :cljs IWithMeta)

  (#?(:clj withMeta, :cljs -with-meta)
    [this meta-map]
    (Multihash. _bytes meta-map _hash)))


(defn create
  "Constructs a new Multihash identifier from the given algorithm key (or
  numeric code) and digest byte array (or hex string)."
  [algorithm ^bytes digest]
  (let [code (if (keyword? algorithm)
               (get codes algorithm)
               algorithm)]
    (when-not (integer? code)
      (throw (ex-info (str (pr-str algorithm) " does not map to a known hash algorithm code.")
               {:algorithm algorithm})))
    (Multihash. (encode-bytes code digest) nil 0)))



;; ## Serialization

(defn- inner-bytes
  "Retrieve the inner encoded bytes from a multihash value."
  [^Multihash mhash]
  (#?(:clj ._bytes :cljs .-_bytes) mhash))


(defn read-bytes
  "Read a multihash from a byte array. Returns a tuple containing the multihash
  and the number of bytes read."
  [^bytes data offset]
  (let [[code csize] (varint/read-bytes data offset)
        [length lsize] (varint/read-bytes data (+ offset csize))
        total-size (+ csize lsize length)
        buffer (b/byte-array total-size)]
    (b/copy data offset buffer 0 total-size)
    [(Multihash. buffer nil 0) total-size]))


(defn write-bytes
  "Write an encoded multihash to a byte array at the given offset. Returns the
  number of bytes written."
  [^Multihash mhash ^bytes buffer offset]
  (let [encoded (inner-bytes mhash)]
    (b/copy encoded 0 buffer offset (alength encoded))
    (alength encoded)))


; TODO: read/write byte streams?


(defn encode
  "Encode a multihash into a binary representation. Returns the byte array."
  ^bytes
  [^Multihash mhash]
  (let [encoded (inner-bytes mhash)
        buffer (b/byte-array (alength encoded))]
    (b/copy encoded 0 buffer 0 (alength encoded))
    buffer))


(defn decode
  "Decode a multihash by reading data from a byte array."
  [^bytes data]
  (first (read-bytes data 0)))


#_
(defn format
  "Format a multihash as a string, using the given base encoding. Returns a
  string _without_ a multibase prefix."
  ^String
  ([mhash]
   (format :base58btc mhash))
  ([base ^Multihash mhash]
   (mbase/format* base (inner-bytes mhash))))


#_
(defn parse
  "Parse a multihash from a string."
  [string]
  ; TODO: how to detect without a multibase prefix? back-compat behavior?
  ,,,)


; TODO: 'hex' and 'base58' back-compat helpers?
; TODO: digest functions?
