(ns multiformats.hash
  "Multihash is a protocol for differentiating outputs from various
  well-established cryptographic hash functions, addressing size and encoding
  considerations.

  https://github.com/multiformats/multihash"
  (:refer-clojure :exclude [test])
  (:require
    [alphabase.base16 :as hex]
    [alphabase.bytes :as b]
    [clojure.string :as str]
    #?@(:cljs
        [[goog.crypt :as crypt]
         [goog.crypt.Md5]
         [goog.crypt.Sha1]
         [goog.crypt.Sha256]
         [goog.crypt.Sha512]])
    [multiformats.varint :as varint])
  #?(:clj
     (:import
       (clojure.lang
         ILookup
         IMeta
         IObj)
       java.io.InputStream
       java.nio.ByteBuffer
       java.security.MessageDigest)))


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


(def ^:private code->algo
  "Mapping of numeric code to algorithm keyword."
  (into {}
        (map (juxt val key))
        codes))


;; ## Coding Functions

(defn- read-header
  "Read the algorithm code and digest bit size from the encoded bytes. Returns
  a tuple of the numeric code, byte length, and number of bytes read."
  [^bytes data]
  (let [[code csize] (varint/read-bytes data 0)
        [length lsize] (varint/read-bytes data csize)]
    [code length (+ csize lsize)]))


(defn- decode-parameters
  "Read the header and digest from the encoded bytes."
  [^bytes data]
  (let [[code length offset] (read-header data)
        digest (str/lower-case (subs (hex/encode data) (* 2 offset)))]
    {:code code
     :algorithm (code->algo code)
     :length length
     :digest digest}))


(defn- encode-bytes
  "Encode a multihash algorithm, digest length, and digest bytes into a single
  byte array."
  [code ^bytes digest]
  (when (or (nil? digest) (zero? (alength digest)))
    (throw (ex-info "Cannot encode a multihash with an empty digest"
                    {:code code})))
  (let [header (b/byte-array 8)
        clength (varint/write-bytes code header 0)
        llength (varint/write-bytes (alength digest) header clength)
        hlength (+ clength llength)
        buffer (b/byte-array (+ hlength (alength digest)))]
    (b/copy header 0 buffer 0 hlength)
    (b/copy digest 0 buffer hlength (alength digest))
    buffer))


(defn- mhash-str
  "Create a string representation of a multihash from a map of parameters."
  [params]
  (let [algo (if-let [algorithm (:algorithm params)]
               (name algorithm)
               (:code params))]
    (str "hash:" algo \: (:digest params))))


;; ## Multihash Type

#?(:bb
   (defrecord Multihash
     [_bytes _meta _hash]

     Object

     (toString
       [this]
       (mhash-str this)))

   :clj
   (deftype Multihash
     [^bytes _bytes
      _meta
      ^:unsynchronized-mutable _hash]

     java.io.Serializable

     Object

     (toString
       [_]
       (mhash-str (decode-parameters _bytes)))


     (equals
       [this that]
       (cond
         (identical? this that) true

         (instance? Multihash that)
         (b/bytes= _bytes (._bytes ^Multihash that))

         :else false))


     (hashCode
       [_]
       (let [hc _hash]
         (if (zero? hc)
           (let [params (decode-parameters _bytes)
                 hc (hash [::multihash (:code params) (:digest params)])]
             (set! _hash hc)
             hc)
           hc)))


     Comparable

     (compareTo
       [this that]
       (cond
         (identical? this that) 0

         (instance? Multihash that)
         (b/compare _bytes (._bytes ^Multihash that))

         :else
         (throw (ex-info
                  (str "Cannot compare multihash value to " (type that))
                  {:this this
                   :that that}))))


     ILookup

     (valAt
       [this k]
       (.valAt this k nil))


     (valAt
       [_ k not-found]
       (case k
         :length (alength _bytes)
         :code (first (read-header _bytes))
         :algorithm (let [[code] (read-header _bytes)]
                      (code->algo code))
         :bits (let [[_ length] (read-header _bytes)]
                 (* length 8))
         :digest (:digest (decode-parameters _bytes))
         not-found))


     IMeta

     (meta
       [_]
       _meta)


     IObj

     (withMeta
       [_ meta-map]
       (Multihash. _bytes meta-map _hash)))

   :cljs
   (deftype Multihash
     [_bytes
      _meta
      ^:unsynchronized-mutable _hash]

     Object

     (toString
       [_]
       (mhash-str (decode-parameters _bytes)))


     IEquiv

     (-equiv
       [this that]
       (cond
         (identical? this that) true

         (instance? Multihash that)
         (b/bytes= _bytes (.-_bytes ^Multihash that))

         :else false))


     IHash

     (-hash
       [_]
       (let [hc _hash]
         (if (zero? hc)
           (let [params (decode-parameters _bytes)
                 hc (hash [::multihash (:code params) (:digest params)])]
             (set! _hash hc)
             hc)
           hc)))


     IComparable

     (-compare
       [this that]
       (cond
         (identical? this that) 0

         (instance? Multihash that)
         (b/compare _bytes (.-_bytes ^Multihash that))

         :else
         (throw (ex-info
                  (str "Cannot compare multihash value to " (type that))
                  {:this this
                   :that that}))))


     ILookup

     (-lookup
       [this k]
       (-lookup this k nil))


     (-lookup
       [_ k not-found]
       (case k
         :length (alength _bytes)
         :code (first (read-header _bytes))
         :algorithm (let [[code] (read-header _bytes)]
                      (code->algo code))
         :bits (let [[_ length] (read-header _bytes)]
                 (* length 8))
         :digest (:digest (decode-parameters _bytes))
         not-found))


     IMeta

     (-meta
       [_]
       _meta)


     IWithMeta

     (-with-meta
       [_ meta-map]
       (Multihash. _bytes meta-map _hash))))


(alter-meta! #'->Multihash assoc :private true)


;; ## Constructors

(defn- resolve-code
  "Resolve an algorithm to a numeric code, or throws an exception on invalid input."
  [algorithm]
  (cond
    (integer? algorithm)
    (if (nat-int? algorithm)
      algorithm
      (throw (ex-info (str "Hash algorithm codes cannot be negative: " algorithm)
                      {:algorithm algorithm})))

    (keyword? algorithm)
    (or (get codes algorithm)
        (throw (ex-info
                 (str algorithm " does not map to a known hash algorithm code.")
                 {:algorithm algorithm})))

    :else
    (throw (ex-info
             (str (pr-str algorithm)
                  " is not a valid algorithm keyword or numeric code.")
             {:algorithm algorithm}))))


(defn- resolve-digest
  "Resolve a digest to a byte array, or throws an exception on invalid input."
  [digest]
  (cond
    (b/bytes? digest)
    digest

    (string? digest)
    (hex/decode digest)

    :else
    (throw (ex-info
             (str (pr-str digest) " is not a byte array or hex string.")
             {:digest digest}))))


(defn create
  "Constructs a new Multihash identifier from the given algorithm key (or
  numeric code) and digest byte array (or hex string)."
  [algorithm digest]
  (let [code (resolve-code algorithm)
        digest-bytes (resolve-digest digest)
        encoded (encode-bytes code digest-bytes)
        mhash (->Multihash encoded nil 0)]
    #?(:bb (assoc mhash
                  :length (count encoded)
                  :digest (str/lower-case (hex/encode digest-bytes))
                  :code code
                  :algorithm (code->algo code)
                  :bits (* 8 (count digest-bytes)))
       :default mhash)))


;; ## Serialization

(defn- inner-bytes
  "Retrieve the inner encoded bytes from a multihash value."
  ^bytes
  [^Multihash mhash]
  (#?(:cljs .-_bytes, :default ._bytes) mhash))


(defn read-bytes
  "Read a multihash from a byte array. Returns a tuple containing the multihash
  and the number of bytes read."
  [^bytes data offset]
  (let [[_ csize] (varint/read-bytes data offset)
        [length lsize] (varint/read-bytes data (+ offset csize))
        total-size (+ csize lsize length)
        buffer (b/byte-array total-size)]
    (b/copy data offset buffer 0 total-size)
    [(Multihash. buffer nil 0) total-size]))


(defn write-bytes
  "Write an encoded multihash to a byte array at the given offset. Returns the
  number of bytes written."
  [^Multihash mhash ^bytes buffer offset]
  (b/copy (inner-bytes mhash) buffer offset))


(defn encode
  "Encode a multihash into a binary representation. Returns the byte array."
  ^bytes
  [^Multihash mhash]
  (b/copy (inner-bytes mhash)))


(defn decode
  "Decode a multihash by reading data from a byte array."
  [^bytes data]
  (first (read-bytes data 0)))


(defn hex
  "Format a multihash as a hex string."
  [mhash]
  (str/lower-case (hex/encode (inner-bytes mhash))))


(defn parse
  "Parse a hex string into a multihash."
  [string]
  (decode (hex/decode string)))


;; ## Digest Constructors

(defn- digest-content
  "Constructs a cryptographic digest for a given hasher and content. Content
  may be in the form of a raw byte array, a `ByteBuffer`, an `InputStream`, or
  a string. Returns a byte array with the digest."
  ^bytes
  [^MessageDigest hasher content]
  (cond
    (string? content)
    (.update hasher (b/from-string content))

    (b/bytes? content)
    (.update hasher ^bytes content)

    #?@(:clj
        [(instance? ByteBuffer content)
         (.update hasher ^ByteBuffer content)

         (instance? InputStream content)
         (let [buffer (byte-array 4096)]
           (loop []
             (let [n (.read ^InputStream content buffer 0 (count buffer))]
               (when (pos? n)
                 (.update hasher buffer 0 n)
                 (recur)))))])

    :else
    (throw (ex-info (str "Don't know how to compute digest from "
                         (type content))
                    {:content content})))
  (.digest hasher))


(defn md5
  "Calculate a multihash of the content using MD5."
  [content]
  (let [hasher #?(:clj (MessageDigest/getInstance "MD5")
                  :cljs (goog.crypt.Md5.))
        digest (digest-content hasher content)]
    (create :md5 digest)))


(defn sha1
  "Calculate a multihash of the content using SHA-1."
  [content]
  (let [hasher #?(:clj (MessageDigest/getInstance "SHA-1")
                  :cljs (goog.crypt.Sha1.))
        digest (digest-content hasher content)]
    (create :sha1 digest)))


(defn sha2-256
  "Calculate a multihash of the content using SHA-256."
  [content]
  (let [hasher #?(:clj (MessageDigest/getInstance "SHA-256")
                  :cljs (goog.crypt.Sha256.))
        digest (digest-content hasher content)]
    (create :sha2-256 digest)))


(defn sha2-512
  "Calculate a multihash of the content using SHA-512."
  [content]
  (let [hasher #?(:clj (MessageDigest/getInstance "SHA-512")
                  :cljs (goog.crypt.Sha512.))
        digest (digest-content hasher content)]
    (create :sha2-512 digest)))


(def functions
  "Map of hash digest functions available."
  {:md5 md5
   :sha1 sha1
   :sha2-256 sha2-256
   :sha2-512 sha2-512})


(defn test
  "Determines whether a multihash is a correct identifier for some content by
  recomputing the digest for the algorithm specified in the multihash. Returns
  nil if either argument is nil, true if the digest matches, or false if not.
  Throws an exception if the multihash specifies an unsupported algorithm."
  [mhash content]
  (when (and mhash content)
    (if-let [hash-fn (get functions (:algorithm mhash))]
      (let [other (hash-fn content)]
        #?(:bb (= (:digest mhash)
                  (:digest other))
           :default (= mhash other)))
      (throw (ex-info
               (str "No supported hashing function for algorithm "
                    (or (:algorithm mhash) (:code mhash))
                    " to validate " mhash)
               {:code (:code mhash)
                :algorithm (:algorithm mhash)})))))
