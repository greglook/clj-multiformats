(ns multiformats.hash
  "Multihash is a protocol for differentiating outputs from various
  well-established cryptographic hash functions, addressing size and encoding
  considerations.

  https://github.com/multiformats/multihash"
  (:refer-clojure :exclude [test])
  (:require
    [alphabase.bytes :as b]
    #?@(:cljs
        [[goog.crypt :as crypt]
         [goog.crypt.Md5]
         [goog.crypt.Sha1]
         [goog.crypt.Sha256]
         [goog.crypt.Sha512]])
    [multiformats.base.b16 :as hex]
    [multiformats.varint :as varint])
  #?(:cljs
     (:require-macros
       [multiformats.hash :refer [defhash]]))
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
  (let [header (b/byte-array 8)
        clength (varint/write-bytes code header 0)
        llength (varint/write-bytes (alength digest) header clength)
        hlength (+ clength llength)
        buffer (b/byte-array (+ hlength (alength digest)))]
    (b/copy header 0 buffer 0 hlength)
    (b/copy digest 0 buffer hlength (alength digest))
    buffer))



;; ## Multihash Type
#?(:bb
   (do
     (defn length
       [mhash]
       (count (:_bytes mhash)))

     (defn digest
       [mhash]
       (:digest (decode-parameters (:_bytes mhash))))

     (defn code
       [mhash]
       (first (read-header (:_bytes mhash))))

     (defn algorithm
       [mhash]
       (let [[code] (read-header (:_bytes mhash))]
         (find-algorithm code)))

     (defn bits
       [mhash]
       (let [[_ length] (read-header (:_bytes mhash))]
         (* length 8)))))

#?(:bb
   (defrecord Multihash
     [_bytes _meta _hash]
     Object
     (toString
       [this]
       (str "hash:" (name (algorithm this)) \: (digest this))))
   :default
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
                 hc (hash [::multihash (:code params) (:digest params)])]
             (set! _hash hc)
             hc)
           hc)))


     #?(:clj Comparable, :cljs IComparable)

     (#?(:clj compareTo, :cljs -compare)
       [this that]
       (cond
         (identical? this that) 0

         (instance? Multihash that)
         (b/compare _bytes (#?(:clj ._bytes :cljs .-_bytes) ^Multihash that))

         :else
         (throw (ex-info
                  (str "Cannot compare multihash value to " (type that))
                  {:this this
                   :that that}))))


     ILookup

     (#?(:clj valAt, :cljs -lookup)
       [this k]
       (#?(:clj .valAt, :cljs -lookup) this k nil))


     (#?(:clj valAt, :cljs -lookup)
       [this k not-found]
       (case k
         :length (alength _bytes)
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
    (hex/parse digest)

    :else
    (throw (ex-info
             (str (pr-str digest) " is not a byte array or hex string.")
             {:digest digest}))))


(defn create
  "Constructs a new Multihash identifier from the given algorithm key (or
  numeric code) and digest byte array (or hex string)."
  [algorithm digest]
  (let [code (resolve-code algorithm)
        digest (resolve-digest digest)]
    (->Multihash (encode-bytes code digest) nil 0)))



;; ## Serialization

(defn- inner-bytes
  "Retrieve the inner encoded bytes from a multihash value."
  ^bytes
  [^Multihash mhash]
  (#?(:clj ._bytes :cljs .-_bytes) mhash))


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
  (hex/format (inner-bytes mhash)))


(defn parse
  "Parse a hex string into a multihash."
  [string]
  (decode (hex/parse string)))



;; ## Digest Constructors

(defn- init-hasher
  "Initialize a hashing algorithm to digest some content. Returns nil if the
  algorithm is not supported on the current platform."
  [algorithm]
  #?(:clj
     (some->
       (case algorithm
         :md5      "MD5"
         :sha1     "SHA-1"
         :sha2-256 "SHA-256"
         :sha2-512 "SHA-512"
         nil)
       (MessageDigest/getInstance))
     :cljs
     (case algorithm
       :md5      (goog.crypt.Md5.)
       :sha1     (goog.crypt.Sha1.)
       :sha2-256 (goog.crypt.Sha256.)
       :sha2-512 (goog.crypt.Sha512.)
       nil)))


(defn- digest-content
  "Constructs a cryptographic digest for a given hasher and content. Content
  may be in the form of a raw byte array, a `ByteBuffer`, an `InputStream`, or
  a string. Returns a byte array with the digest."
  ^bytes
  [^MessageDigest hasher content]
  (cond
    (string? content)
    (let [utf8-bytes #?(:clj (.getBytes ^String content)
                        :cljs (crypt/stringToUtf8ByteArray content))]
      (.update hasher ^bytes utf8-bytes))

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


(defmacro ^:private defhash
  "Defines a new hashing function for the given algorithm."
  [algo-sym]
  `(defn ~algo-sym
     ~(str "Calculates the " algo-sym
           " digest of the given content and returns a multihash.")
     [~'content]
     (let [algo-key# ~(keyword algo-sym)
           hasher# (init-hasher algo-key#)
           digest# (digest-content hasher# ~'content)]
       (create algo-key# digest#))))


;; Appease clj-kondo
(declare md5 sha1 sha2-256 sha2-512)


(defhash md5)
(defhash sha1)
(defhash sha2-256)
(defhash sha2-512)


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
  (let [algo #?(:bb algorithm :default :algorithm)
        code #?(:bb code :default :code)]
    (when (and mhash content)
      (if-let [hasher (init-hasher (algo mhash))]
        (let [digest (digest-content hasher content)
              other (create (code mhash) digest)]
          #?(:bb (= (multiformats.hash/digest mhash)
                    (multiformats.hash/digest other))
             :default (= mhash other)))
        (throw (ex-info
                 (str "No supported hashing function for algorithm "
                      (or (algo mhash) (:code mhash))
                      " to validate " mhash)
                 {:code (:code mhash)
                  :algorithm (algo mhash)}))))))
