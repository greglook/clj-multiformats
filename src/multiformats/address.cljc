(ns multiformats.address
  "Multiaddr aims to make network addresses future-proof, composable, and
  efficient.

  An `Address` behaves like a sequence of `[protocol-key value]` pairs which
  you can `seq` through. This isn't an associative structure on protocol keys
  since you can have duplicate pairs and the order of the (key, val) pairs
  matters (e.g, for cases like tunneling).

  The binary form of an address entry consists of one to three values:
  - A varint encoding the numeric protocol code
  - An optional varint with the number of bytes in a variable-length value (e.g. a UTF-8 string)
  - The bytes encoding the protocol value, if any

  See: https://github.com/multiformats/multiaddr"
  (:require
    [alphabase.bytes :as b]
    [clojure.string :as str]
    #?@(:cljs [[goog.crypt :as crypt]
               [goog.net.ipaddress :as ipaddress]])
    [multiformats.varint :as varint])
  #?(:clj
     (:import
       (clojure.lang
         Counted
         IMeta
         IObj
         IPersistentCollection
         IPersistentStack
         Indexed
         Seqable
         Sequential)
       java.io.Serializable
       (java.net
         Inet4Address
         Inet6Address
         InetAddress))))


;; ## Protocol Definitions

(def protocols
  "Map from address protocol keywords to a map of information about it.

  https://github.com/multiformats/multiaddr/blob/master/protocols.csv"
  (into {}
        (map (fn prep-protocol
               [proto]
               [(first proto) (zipmap [:key :code :type :desc] proto)]))
        [[:ip4             4 :ip4]
         [:tcp             6 :ushort]
         [:udp           273 :ushort]
         [:dccp           33 :ushort]
         [:ip6            41 :ip6]
         [:ip6zone        42 :utf8 "rfc4007 IPv6 zone"]
         [:dns            53 :utf8 "domain name resolvable to both IPv6 and IPv4 addresses"]
         [:dns4           54 :utf8 "domain name resolvable only to IPv4 addresses"]
         [:dns6           55 :utf8 "domain name resolvable only to IPv6 addresses"]
         [:dnsaddr        56 :utf8]
         [:sctp          132 :ushort]
         [:udt           301 :null]
         [:utp           302 :null]
         [:unix          400 :utf8]
         [:p2p           421 :utf8 "preferred over /ipfs"]
         [:ipfs          421 :utf8 "backwards compatibility; equivalent to /p2p"]
         [:garlic64      446 :utf8]
         [:garlic32      447 :utf8]
         [:tls           448 :null "Transport Layer Security"]
         [:sni           449 :utf8 "Server Name Indication RFC 6066 § 3"]
         [:noise         454 :null]
         [:quic          460 :null]
         [:quic-v1       461 :null]
         [:webtransport  465 :null]
         [:certhash      466 :utf8]
         [:http          480 :null "HyperText Transfer Protocol"]
         [:http-path     481 :utf8 "Percent-encoded path to an HTTP resource"]
         [:https         443 :null "Deprecated alias for /tls/http"]
         [:ws            477 :null "WebSockets"]
         [:wss           478 :null "Deprecated alias for /tls/ws"]
         [:webrtc-direct 280 :null "ICE-lite webrtc transport with SDP munging during connection establishment and without use of a STUN server"]
         [:webrtc        281 :null "webrtc transport where connection establishment is according to w3c spec"]
         [:p2p-circuit   290 :null]
         [:memory        777 :utf8 "in memory transport for self-dialing and testing; arbitrary"]]))


(def ^:private code->protocol
  "Mapping from code point to protocol keyword."
  (into {}
        (map (juxt :code :key))
        (vals protocols)))


;; ## String Functions

(defn- format-entry
  "Format an address protocol entry into a segment in a human-readable string."
  [[proto-key value]]
  (if (some? value)
    (str "/" (name proto-key) "/" value)
    (str "/" (name proto-key))))


(defn- format-entries
  "Format an entire address string from a sequence of protocol entries."
  [entries]
  (str/join (map format-entry entries)))


(defn- parse-entries
  "Parse a formatted string into a sequence of protocol+value entries."
  [string]
  (when-not (str/starts-with? string "/")
    (throw (ex-info "Expected address string to begin with a slash"
                    {:address string})))
  (loop [parts (rest (str/split string #"/"))
         entries []]
    (if (seq parts)
      ;; Parse next protocol entry
      (let [proto-str (first parts)]
        (when-not (re-matches #"[a-z][a-z0-9-]+" proto-str)
          (throw (ex-info (str "Invalid protocol type string: " proto-str)
                          {:address string
                           :protocol proto-str})))
        (let [protocol (or (get protocols (keyword proto-str)))]
          (when-not protocol
            (throw (ex-info (str "Unknown protocol type: " proto-str)
                            {:address string
                             :protocol proto-str})))
          (if (= :null (:type protocol))
            (recur (next parts)
                   (conj entries (:key protocol)))
            (let [value (second parts)]
              (when-not value
                (throw (ex-info (str "Missing value for " proto-str " protocol")
                                {:address string
                                 :protocol (:key protocol)})))
              (recur (nnext parts)
                     (conj entries [(:key protocol) value]))))))
      ;; No more parts to parse
      entries)))


;; ## Binary Encoding

(defn- encode-utf8-entry
  "Encode a protocol entry for a UTF-8 string value to a sequence of byte
  arrays."
  [protocol value]
  (let [proto-key (:key protocol)
        str-bytes (if (string? value)
                    #?(:clj (.getBytes ^String value "UTF8")
                       :cljs (js/Uint8Array. (crypt/stringToByteArray value)))
                    (throw (ex-info (str "Protocol " (name proto-key)
                                         " requires a UTF-8 string value, got: "
                                         (pr-str value))
                                    {:protocol proto-key
                                     :value value})))]
    [(varint/encode (:code protocol))
     (varint/encode (alength ^bytes str-bytes))
     str-bytes]))


(defn- encode-short-entry
  "Encode a protocol entry for an unsigned short value to a sequence of byte
  arrays."
  [protocol value]
  (let [proto-key (:key protocol)
        n (cond
            (integer? value)
            value

            (string? value)
            (or (parse-long value)
                (throw (ex-info (str "Protocol " (name proto-key)
                                     " has invalid string value: " value)
                                {:protocol proto-key
                                 :value value})))

            :else
            (throw (ex-info (str "Protocol " (name proto-key)
                                 " requires an unsigned short value or numeric string, got: "
                                 (pr-str value))
                            {:protocol proto-key
                             :value value})))]
    (when (< 65535 n)
      (throw (ex-info (str "Protocol " (name proto-key)
                           " has value too big for unsigned short: " n)
                      {:protocol proto-key
                       :value n})))
    [(varint/encode (:code protocol))
     (b/init-bytes
       [(bit-and (bit-shift-right n 8) 0xFF)
        (bit-and n 0xFF)])]))


(defn- encode-ip4-entry
  "Encode a protocol entry for an IPv4 address value to a sequence of byte
  arrays."
  [protocol value]
  (let [proto-key (:key protocol)
        addr-bytes (cond
                     (string? value)
                     #?(:clj
                        (try
                          (let [addr (InetAddress/getByName value)]
                            (when-not (instance? Inet4Address addr)
                              (throw (ex-info (str "Invalid IPv4 address string: " value)
                                              {:protocol proto-key
                                               :value value})))
                            (.getAddress addr))
                          (catch Exception ex
                            (throw (ex-info (str "Invalid IPv4 address string: " value)
                                            {:protocol proto-key
                                             :value value}
                                            ex))))

                        :cljs
                        (let [addr (ipaddress/IpAddress.fromString value)]
                          (when (or (nil? addr) (not= 4 (.getVersion addr)))
                            (throw (ex-info (str "Invalid IPv4 address string: " value)
                                            {:protocol proto-key
                                             :value value})))
                          (let [bs (b/byte-array 4)
                                addr-ints (.toInteger addr)
                                n (.getBitsUnsigned addr-ints 0)]
                            ;; goog.math.Integer int32 pieces are little endian
                            ;; so need to work backwards for network order.
                            (b/set-byte bs 0 (bit-and (bit-shift-right n 24) 0xFF))
                            (b/set-byte bs 1 (bit-and (bit-shift-right n 16) 0xFF))
                            (b/set-byte bs 2 (bit-and (bit-shift-right n 8) 0xFF))
                            (b/set-byte bs 3 (bit-and n 0xFF))
                            bs)))

                     ;; TODO: could accept "real" IP value types here

                     :else
                     (throw (ex-info (str "Protocol " (name proto-key)
                                          " requires an IP address string, got: "
                                          (pr-str value))
                                     {:protocol proto-key
                                      :value value})))]
    [(varint/encode (:code protocol))
     addr-bytes]))


(defn- encode-ip6-entry
  "Encode a protocol entry for an IPv6 address value to a sequence of byte
  arrays."
  [protocol value]
  (let [proto-key (:key protocol)
        addr-bytes (cond
                     (string? value)
                     #?(:clj
                        (try
                          (let [addr (InetAddress/getByName value)]
                            (when-not (instance? Inet6Address addr)
                              (throw (ex-info (str "Invalid IPv6 address string: " value)
                                              {:protocol proto-key
                                               :value value})))
                            (.getAddress addr))
                          (catch Exception ex
                            (throw (ex-info (str "Invalid IPv6 address string: " value)
                                            {:protocol proto-key
                                             :value value}
                                            ex))))

                        :cljs
                        (let [addr (ipaddress/IpAddress.fromString value)]
                          (when (or (nil? addr) (not= 6 (.getVersion addr)))
                            (throw (ex-info (str "Invalid IPv6 address string: " value)
                                            {:protocol proto-key
                                             :value value})))
                          (let [bs (b/byte-array 16)
                                addr-ints (.toInteger addr)]
                            ;; goog.math.Integer int32 pieces are little endian
                            ;; so need to work backwards for network order.
                            (dotimes [idx 4]
                              (let [n (.getBitsUnsigned addr-ints (- 3 idx))
                                    offset (* 4 idx)]
                                (b/set-byte bs (+ offset 0) (bit-and (bit-shift-right n 24) 0xFF))
                                (b/set-byte bs (+ offset 1) (bit-and (bit-shift-right n 16) 0xFF))
                                (b/set-byte bs (+ offset 2) (bit-and (bit-shift-right n 8) 0xFF))
                                (b/set-byte bs (+ offset 3) (bit-and n 0xFF))))
                            bs)))

                     ;; TODO: could accept "real" IP value types here

                     :else
                     (throw (ex-info (str "Protocol " (name proto-key)
                                          " requires an IP address string, got: "
                                          (pr-str value))
                                     {:protocol proto-key
                                      :value value})))]
    [(varint/encode (:code protocol))
     addr-bytes]))


(defn- encode-entry
  "Encode a protocol entry to a sequence of byte arrays that represent the
  entry when concatenated together."
  [entry]
  (when-not (or (keyword? entry)
                (and (vector? entry)
                     (keyword? (first entry))
                     (or (= 1 (count entry))
                         (= 2 (count entry)))))
    (throw (ex-info "Address entry must be a protocol keyword or vector pair"
                    {:entry entry})))
  (let [[proto-key value] (if (keyword? entry)
                            [entry nil]
                            entry)
        protocol (get protocols proto-key)]
    (when-not protocol
      (throw (ex-info (str "Unsupported protocol type: " (name proto-key))
                      {:protocol proto-key})))
    (case (:type protocol)
      :null
      (if (nil? value)
        [(varint/encode (:code protocol))]
        (throw (ex-info (str "Protocol " (name proto-key) " does not support values, got: " (pr-str value))
                        {:protocol proto-key
                         :value value})))

      :utf8
      (encode-utf8-entry protocol value)

      :ushort
      (encode-short-entry protocol value)

      :ip4
      (encode-ip4-entry protocol value)

      :ip6
      (encode-ip6-entry protocol value))))


;; ## Binary Decoding

(defn- decode-utf8-entry
  "Decode a protocol entry for a UTF-8 string value from the offset into the
  byte array. Returns a vector with the value and the number of bytes read."
  [^bytes data offset]
  (let [[str-len len-len] (varint/read-bytes data offset)
        string #?(:clj
                  (String. data (int (+ offset len-len)) (int str-len) "UTF-8")
                  :cljs
                  (crypt/utf8ByteArrayToString
                    (b/copy-slice data (+ offset len-len) str-len)))]
    [string (+ len-len str-len)]))


(defn- decode-short-entry
  "Decode a protocol entry for an unsigned short value from the offset into the
  byte array. Returns a vector with the value and the number of bytes read."
  [^bytes data offset]
  (let [n (bit-or (bit-shift-left (b/get-byte data offset) 8)
                  (b/get-byte data (inc offset)))]
    [n 2]))


(defn- decode-ip4-entry
  "Decode a protocol entry for an IPv4 address value from the offset into the
  byte array. Returns a vector with the value and the number of bytes read."
  [^bytes data offset]
  (let [addr #?(:clj
                (->
                  (b/copy-slice data offset 4)
                  (InetAddress/getByAddress)
                  (.getHostAddress))
                ;; goog.net.IpAddress doesn't support an easy way
                ;; to build IP from bytes, so manually construct string
                :cljs
                (str (b/get-byte data offset) "."
                     (b/get-byte data (+ offset 1)) "."
                     (b/get-byte data (+ offset 2)) "."
                     (b/get-byte data (+ offset 3))))]
    [addr 4]))


(defn- decode-ip6-entry
  "Decode a protocol entry for an IPv6 address value from the offset into the
  byte array. Returns a vector with the value and the number of bytes read."
  [^bytes data offset]
  (let [addr #?(:clj
                (->
                  (b/copy-slice data offset 16)
                  (InetAddress/getByAddress)
                  (.getHostAddress))
                ;; goog.net.IpAddress doesn't support an easy way
                ;; to build IP from bytes, so manually construct string
                :cljs
                (->>
                  (range 8)
                  (map (fn extract-hextet
                         [i]
                         (let [hex-off (+ offset (* 2 i))
                               n (bit-or
                                   (bit-shift-left (b/get-byte data hex-off) 8)
                                   (b/get-byte data (inc hex-off)))]
                           (.toString n 16))))
                  (str/join ":")))]
    [addr 16]))


(defn- decode-entry
  "Decode a protocol entry from the offset into the byte array. Returns a
  vector with the protocol entry and its length in bytes."
  [^bytes data offset]
  (let [[code code-len] (varint/read-bytes data offset)
        proto-key (code->protocol code)
        protocol (get protocols proto-key)]
    (when-not protocol
      (throw (ex-info (str "Unsupported protocol code: " code)
                      {:code code
                       :offset offset})))
    (case (:type protocol)
      :null
      [[proto-key] code-len]

      :utf8
      (let [[string val-len] (decode-utf8-entry data (+ offset code-len))]
        [[proto-key string] (+ code-len val-len)])

      :ushort
      (let [[n val-len] (decode-short-entry data (+ offset code-len))]
        [[proto-key n] (+ code-len val-len)])

      :ip4
      (let [[addr val-len] (decode-ip4-entry data (+ offset code-len))]
        [[proto-key addr] (+ code-len val-len)])

      :ip6
      (let [[addr val-len] (decode-ip6-entry data (+ offset code-len))]
        [[proto-key addr] (+ code-len val-len)]))))


(defn- decode-entries
  "Decode a sequence of entries from the byte array."
  [^bytes data]
  (loop [offset 0
         entries []]
    (if (< offset (alength data))
      (let [[entry entry-len] (decode-entry data offset)]
        (recur (long (+ offset entry-len)) (conj entries entry)))
      entries)))


;; ## Address Type

#?(:clj
   (deftype Address
     [^bytes _bytes
      _points
      _meta
      ^:unsynchronized-mutable _hash]

     Serializable


     Object

     (toString
       [_]
       (format-entries (decode-entries _bytes)))


     (equals
       [this that]
       (cond
         (identical? this that)
         true

         (instance? Address that)
         (b/bytes= _bytes (._bytes ^Address that))

         :else
         false))


     (hashCode
       [_]
       (if (zero? _hash)
         (let [hc (hash (b/byte-seq _bytes))]
           (set! _hash hc)
           hc)
         _hash))


     Comparable

     (compareTo
       [this that]
       (cond
         (identical? this that)
         0

         (instance? Address that)
         (b/compare _bytes (.-_bytes ^Address that))

         :else
         (throw (ex-info
                  (str "Cannot compare multiaddress value to " (type that))
                  {:this this
                   :that that}))))


     IMeta

     (meta
       [_]
       _meta)


     IObj

     (withMeta
       [_ new-meta]
       (Address. _bytes _points new-meta _hash))


     Counted

     (count
       [_]
       (count _points))


     Sequential


     Seqable

     (seq
       [_]
       (seq (decode-entries _bytes)))


     Indexed

     (nth
       [_ i]
       (if (<= 0 i (dec (count _points)))
         (let [offset (nth _points i)]
           (first (decode-entry _bytes offset)))
         (throw (IndexOutOfBoundsException.
                  (str "Index " i " is outside the "
                       (count _points) " elements in the address")))))


     (nth
       [_ i not-found]
       (if (<= 0 i (dec (count _points)))
         (let [offset (nth _points i)]
           (first (decode-entry _bytes offset)))
         not-found))


     IPersistentCollection

     (empty
       [_]
       (Address. (b/byte-array 0) [] _meta 0))


     (equiv
       [this that]
       (.equals this that))


     (cons
       [_ entry]
       (let [entry-arrs (encode-entry entry)
             entry-len (apply + (map alength entry-arrs))
             new-bytes (apply b/concat (cons _bytes entry-arrs))
             new-points (conj _points (alength _bytes))]
         (Address. new-bytes new-points _meta 0)))


     IPersistentStack

     (peek
       [_]
       (when (seq _points)
         (let [offset (peek _points)]
           (first (decode-entry _bytes offset)))))


     (pop
       [_]
       (when (empty? _points)
         (throw (ex-info "Can't pop empty address" {})))
       (let [offset (peek _points)
             new-bytes (b/copy-slice _bytes 0 offset)
             new-points (pop _points)]
         (Address. new-bytes new-points _meta 0))))

   :cljs
   (deftype Address
     [_bytes
      _points
      _meta
      ^:unsynchronized-mutable _hash]

     Object

     (toString
       [_]
       (format-entries (decode-entries _bytes)))


     IEquiv

     (-equiv
       [this that]
       (cond
         (identical? this that)
         true

         (instance? Address that)
         (b/bytes= _bytes (.-_bytes ^Address that))

         :else
         false))


     IHash

     (-hash
       [_]
       (if (zero? _hash)
         (let [hc (hash (b/byte-seq _bytes))]
           (set! _hash hc)
           hc)
         _hash))


     IComparable

     (-compare
       [this that]
       (cond
         (identical? this that)
         0

         (instance? Address that)
         (b/compare _bytes (.-_bytes ^Address that))

         :else
         (throw (ex-info
                  (str "Cannot compare multiaddress value to " (type that))
                  {:this this
                   :that that}))))


     IMeta

     (-meta
       [_]
       _meta)


     IWithMeta

     (-with-meta
       [_ new-meta]
       (Address. _bytes _points new-meta _hash))


     ICounted

     (-count
       [_]
       (count _points))


     ISequential


     ISeqable

     (-seq
       [_]
       (seq (decode-entries _bytes)))


     IIndexed

     (-nth
       [this i]
       (if (<= 0 i (dec (count _points)))
         (let [offset (nth _points i)]
           (first (decode-entry _bytes offset)))
         (throw (ex-info
                  (str "Index " i " is outside the "
                       (count _points) " elements in the address")
                  {:i i}))))


     (-nth
       [_ i not-found]
       (if (<= 0 i (dec (count _points)))
         (let [offset (nth _points i)]
           (first (decode-entry _bytes offset)))
         not-found))


     IEmptyableCollection

     (-empty
       [_]
       (Address. (b/byte-array 0) [] _meta 0))


     ICollection

     (-conj
       [_ entry]
       (let [entry-arrs (encode-entry entry)
             entry-len (apply + (map alength entry-arrs))
             new-bytes (apply b/concat (cons _bytes entry-arrs))
             new-points (conj _points (alength _bytes))]
         (Address. new-bytes new-points _meta 0)))


     IStack

     (-peek
       [_]
       (when (seq _points)
         (let [offset (peek _points)]
           (first (decode-entry _bytes offset)))))


     (-pop
       [_]
       (when (empty? _points)
         (throw (ex-info "Can't pop empty address" {})))
       (let [offset (peek _points)
             new-bytes (b/copy-slice _bytes 0 offset)
             new-points (pop _points)]
         (Address. new-bytes new-points _meta 0)))))


(alter-meta! #'->Address assoc :private true)


;; ## Constructors

(defn create
  "Constructs a new multiaddr from a sequence of protocol/value pairs. Each
  entry should be a vector with a protocol keyword and a value; alternatively,
  protocols with no value may be specified as a simple keyword. The values
  should be either strings or appropriate types, such as numbers or IP
  addresses.

      (create [[:ip4 \"127.0.0.1\"] [:tcp 80] :tls])"
  ([]
   (create nil))
  ([entries]
   (let [entry-bytes (mapv encode-entry entries)
         points (loop [offset 0
                       points []
                       entry-bytes entry-bytes]
                  (if (seq entry-bytes)
                    (let [next-len (apply + (map alength (first entry-bytes)))]
                      (recur (long (+ offset next-len))
                             (conj points offset)
                             (next entry-bytes)))
                    points))
         addr-bytes (apply b/concat (apply concat entry-bytes))]
     (->Address addr-bytes points nil 0))))


(defn address?
  "True if the value is a multiaddress object."
  [x]
  (instance? Address x))


;; ## Serialization

(defn encode
  "Encode a multiaddr into a binary representation. Returns the byte array."
  ^bytes
  [^Address addr]
  (b/copy (.-_bytes addr)))


(defn decode
  "Decode a multiaddr by reading data from a byte array."
  ^Address
  [^bytes data]
  (when (seq data)
    (loop [offset 0
           points []]
      (if (< offset (alength data))
        (let [[entry entry-len] (decode-entry data offset)]
          (recur (long (+ offset entry-len))
                 (conj points offset)))
        (->Address (b/copy data) points nil 0)))))


(defn parse
  "Parse a string representation into a multiaddr.

      (parse \"/ip4/127.0.0.1/tcp/80/tls\")"
  ^Address
  [string]
  (create (parse-entries string)))
