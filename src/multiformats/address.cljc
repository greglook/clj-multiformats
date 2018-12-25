(ns multiformats.address
  "Multiaddr aims to make network addresses future-proof, composable, and
  efficient.

  `Address` behaves like a collection of [protocol-key value] pairs, which you
  can `seq` through or `conj` on new [key value] pairs.

  This isn't an associative structure on protocol-keys since you can
  have duplicate pairs and the order of the (key, val) pairs matters
  (e.g, for cases like tunneling).


  See https://github.com/multiformats/multiaddr for more info on format"
  (:require
   [alphabase.bytes :as b]
   [clojure.string :as str]
   [multiformats.address.codec :as codec]
   [multiformats.varint :as varint])
  #?(:clj
     (:import
      (clojure.lang
       Seqable
       IObj
       IMeta
       IPersistentCollection))))

(def protocol->attrs
  "Map from address protocols to attributes used in encoding/decoding

  https://github.com/multiformats/multiaddr/blob/master/protocols.csv"
  {:ip4                {:code 0x04   :codec codec/ip4-codec}
   :tcp                {:code 0x06   :codec codec/ushort-codec}
   :udp                {:code 0x0111 :codec codec/ushort-codec}
   :dccp               {:code 0x21   :codec codec/ushort-codec}
   :ip6                {:code 0x21   :codec codec/ip6-codec}
   :quic               {:code 0x01CC :codec codec/no-value-codec}
   :udt                {:code 0x012D :codec codec/no-value-codec}
   :utp                {:code 0x012E :codec codec/no-value-codec}
   :ws                 {:code 0x01DD :codec codec/no-value-codec}
   :wss                {:code 0x01DE :codec codec/no-value-codec}
   :p2p-websocket-star {:code 0x1DF  :codec codec/no-value-codec}
   :p2p-webrtc-star    {:code 0x113  :codec codec/no-value-codec}
   :p2p-webrtc-direct  {:code 0x114  :codec codec/no-value-codec}
   :p2p-circuit        {:code 0x0122 :codec codec/no-value-codec}
   :dns                {:code 0x35   :codec codec/utf8-codec}
   :dns4               {:code 0x36   :codec codec/utf8-codec}
   :dns6               {:code 0x37   :codec codec/utf8-codec}
   :dnsaddr            {:code 0x38   :codec codec/utf8-codec}})

(def protocol->code
  (into {} (map (juxt key (comp :code val)) protocol->attrs)))

(def code->protocol
  (into {} (map (juxt val key) protocol->code)))

(defn- ensure-protocol-attrs
  "look up protocol attributes, throws if unknown"
  [protocol-key]
  (let [protocol-attrs (get protocol->attrs protocol-key)]
    (when-not protocol-attrs
      (throw (ex-info (str "No protocol associated with " protocol-key)
                      {:protocol protocol-key})))
    protocol-attrs))

(defn- copy-slice
  ([src offset len]
   (let [dst (b/byte-array len)]
     (b/copy src offset dst 0 len)
     dst))
  ([src offset]
   (copy-slice src offset (- (alength ^bytes src) offset))))

(defn- decode-value
  "decode value using protocol `codec` in `data` starting at `offset`"
  [codec ^bytes data offset]
  (if-let [fixed-len (codec/fixed-byte-length codec)]
    (let [value-bytes (copy-slice data offset fixed-len)]
      [(codec/bytes->str codec value-bytes) fixed-len])
    ;; varint encoding
    (let [[value-len num-read] (varint/read-bytes data offset)
          value-bytes (copy-slice data (+ offset num-read) value-len)]
      [(codec/bytes->str codec value-bytes) (+ num-read value-len)])))

(defn- protocol-value-seq
  "lazy sequence of `[protocol-key value]`, where
  `value` is a string representation of the address protocol
   value of `nil` if protocol has no value"
  ([^bytes data]
   (protocol-value-seq data 0))
  ([^bytes data offset]
   (when (< offset (alength data))
     (lazy-seq
      (let [[code code-len] (varint/read-bytes data offset)
            protocol-key (get code->protocol code)
            codec (:codec (ensure-protocol-attrs protocol-key))
            val-start (+ offset code-len)
            [value num-read] (decode-value codec data val-start)]
        (cons [protocol-key value]
              (protocol-value-seq data (+ offset code-len num-read))))))))

(defn- concat-arrs [& arrs]
  (let [arrs (remove nil? arrs)
        total-len (reduce + (map alength arrs))
        dst (b/byte-array total-len)]
    (loop [arrs arrs offset 0]
      (when-let [^bytes src (first arrs)]
        (b/copy src 0 dst offset (alength src))
        (recur (next arrs) (+ offset (alength src)))))
    dst))

(defn- entry-bytes
  "build byte array representation for protocol/value pair,
   note that `value` may be nil for no-value protocols.

   entry can either be a single keyword for a no-value protocol, or a
   pair of [protocol-key value]. You can also pass in [key nil] for a protocol
   not expecting a value"
  [entry]
  (let [protocol-key (if (keyword? entry) entry (first entry))
        value (when (coll? entry) (second entry))
        {:keys [code, codec]} (ensure-protocol-attrs protocol-key)
        code-bytes (varint/encode code)
        fixed-len (codec/fixed-byte-length codec)
        ;; no value bytes needed for no-value protocols (fixed-length at 0)
        val-bytes (when (or (not fixed-len) (pos? fixed-len))
                    (codec/str->bytes codec value))
        ;; only have length bytes for variable length protocols
        val-len-bytes (when-not fixed-len
                        (varint/encode (alength ^bytes val-bytes)))]
    (concat-arrs code-bytes val-len-bytes val-bytes)))

(deftype Address
  [^bytes _data _meta ^:unsynchronized-mutable _hash]

  #?(:clj Seqable :cljs ISeqable)
  (#?(:clj seq :cljs -seq) [this] (protocol-value-seq _data))

  #?(:clj IPersistentCollection)

  #?(:cljs ICounted)
  (#?(:clj count :cljs -count)
    [this] (count (seq this)))

  #?(:cljs IEmptyableCollection)
  (#?(:clj empty :cljs -empty)
    [this] (Address. (b/byte-array 0) _meta 0))

  #?(:cljs IEquiv)
  (#?(:clj equiv :cljs -equiv) [this other]
    (and (instance? Address other)
         (b/bytes= (.-_data this) (.-_data ^Address other))))

  #?(:cljs ICollection)
  (#?(:clj cons :cljs -conj) [this entry]
    (Address. (concat-arrs _data (entry-bytes entry)) _meta 0))

  IMeta
  (#?(:clj meta, :cljs -meta)
    [this]
    _meta)

  #?(:clj IObj :cljs IWithMeta)
  (#?(:clj withMeta :cljs -with-meta)
    [this new-meta]
    (Address. _data new-meta _hash))

  #?(:cljs IHash)
  (#?(:clj hashCode, :cljs -hash)
    [this]
    (if (zero? _hash)
      (let [hc (hash (b/byte-seq _data))]
        (set! _hash hc)
        hc)
      _hash))

  Object
  (toString [this]
    (->> this
         (mapcat (fn [[k v]] [(name k) v]))
         (remove nil?)
         (str/join "/")
         (str "/"))))

(defn- parse-entries
  [address-str]
  (loop [parts (->> #"/" (str/split address-str) (remove str/blank?))
         pairs []]
    (if-let [protocol-key (-> parts first keyword)]
      (let [{:keys [code, codec] :as attrs}
            (ensure-protocol-attrs protocol-key)
            fixed-len (codec/fixed-byte-length codec)]
        (if (and fixed-len (zero? fixed-len))
          (recur (next parts) (conj pairs [protocol-key nil]))
          (let [value (second parts)]
            (when-not value
              (throw (ex-info (str "Missing value for  " (name protocol-key))
                              {:protocol protocol-key :address address-str})))
            (recur (nnext parts)
                   (conj pairs [protocol-key (str value)])))))
      pairs)))

(defn parse
  "parse a human-readable multiaddr string; the data that backs
   the result is a packed representation.

  The `Address` result can be treated as a collection over
  the [protocol-key value] pairs that you can seq over
  or directly `(conj addr [key val])` pairs unto
  "
  ^Address [address-str]
  (let [pairs (parse-entries address-str)
        pair-bytes (map entry-bytes pairs)]
    (->Address (apply concat-arrs pair-bytes) nil 0)))

(defn create
  "create address from entries (possibly empty) you can build on

  (str (create [[:ip4 \"127.0.0.1\"][:tcp \"80\"]]))
  >> \"/ip4/127.0.0.1/tcp/80\" "
  [& entries]
  (let [addr-bytes (apply concat-arrs (map entry-bytes entries))]
    (->Address addr-bytes nil 0)))

(defn encode
  "returns byte array representation of address"
  ^bytes [^Address addr]
  (b/copy (.-_data addr)))

(defn decode
  "Decode address from byte array"
  ^Address [^bytes data]
  (Address. data nil 0))
