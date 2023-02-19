(ns multiformats.address.codec
  "Internal namespace for parsing/formatting multiaddr formats"
  (:require
    [alphabase.bytes :as b]
    #?@(:cljs [[clojure.string :as str]
               [goog.crypt :as crypt]
               [goog.net.ipaddress :as ipaddress]]))
  #?(:clj
     (:import
       java.net.InetAddress)))


(defprotocol Codec
  "Convert multiaddr protocol values between
   human-readable string and machine-readable bytes"

  (str->bytes
    [this value]
    "convert readable value to packed representation")

  (bytes->str
    [this ^bytes data]
    "convert machine representation slice to readable value")

  (fixed-byte-length
    [this]
    "returns fixed length byte length of protocol (if exists),
     including 0 for no value protocols. Can also return `nil`
     if length is variable int encoded"))


(defn- big-endian-bytes->uint
  ([^bytes data]
   (big-endian-bytes->uint data 0 (alength data)))
  ([^bytes data start-offset len]
   (loop [n 0 idx 0]
     (if (= idx len)
       n
       (let [offset (+ start-offset idx)
             rev-idx (dec (- len idx))
             b (b/get-byte data offset)
             raw-mask (bit-shift-left b (* 8 rev-idx))
             ;; handle negative byte as unsigned byte
             mask (if (neg? raw-mask) (bit-and raw-mask 0xff) raw-mask)]
         (recur (bit-or n mask) (inc idx)))))))


(defn- str->int
  ([string radix]
   (#?(:clj Integer/parseInt :cljs js/parseInt) string radix))
  ([string] (str->int string 10)))


(defn- int->str
  [n radix]
  #?(:clj  (Integer/toString ^long n ^long radix)
     :cljs (.toString n ^int radix)))


(defn- write-uint-bytes!
  [n ^bytes data offset num-bytes]
  (when (neg? n)
    (throw (ex-info "Number must be non-negative" {:n n})))
  (dotimes [idx num-bytes]
    (let [rev-idx (dec (- num-bytes idx))
          b (bit-and (bit-shift-right n (* 8 rev-idx)) 0xff)]
      (b/set-byte data (+ offset idx) b))))


(defrecord IPAddressCodec
  [version radix separator num-components bytes-per-component]

  Codec

  (str->bytes
    [_ addr]
    #?(:clj
       (try
         (-> ^String addr InetAddress/getByName .getAddress)
         (catch Exception inner-exception
           (throw (ex-info (str "Value doesn't conform to IPv" version)
                           {:value addr
                            :inner-exception inner-exception
                            :ip-version version}))))
       :cljs
       (let [total-bytes (* num-components bytes-per-component)
             dst (b/byte-array total-bytes)
             num-pieces (/ total-bytes 4)
             ;; goog.math.Integer is array of int32 under the hood
             ip (ipaddress/IpAddress.fromString addr)]
         (when (or (nil? ip) (not= version (.getVersion ip)))
           (throw (ex-info (str "Value doesn't conform to IPv" version)
                           {:value addr :ip-version version})))
         (let [addr-num (.toInteger ip)]
           (dotimes [piece-idx num-pieces]
             ;; goog.math.Integer int32 pieces are little endian
             ;; so need to iterate backwards for network order
             (let [rev-piece-idx (dec (- num-pieces piece-idx))
                   piece (.getBitsUnsigned addr-num rev-piece-idx)
                   piece-offset (* 4 piece-idx)]
               (write-uint-bytes! piece dst piece-offset 4))))
         dst)))


  (bytes->str
    [_ data]
    (when (not= (alength ^bytes data) (* num-components bytes-per-component))
      (throw (ex-info (str "Incorrect bytes for IPv" version)
                      {:value (b/byte-seq data)
                       :ip-version version})))
    #?(:clj
       (-> ^bytes data InetAddress/getByAddress .getHostAddress)
       :cljs
       ;; goog.net.IpAddress doesn't support an easy way
       ;; to build IP from bytes, so manually construct string
       (str/join separator
                 (for [idx (range num-components)
                       :let [offset (* idx bytes-per-component)]]
                   (-> data
                       (big-endian-bytes->uint offset bytes-per-component)
                       (int->str radix))))))


  (fixed-byte-length
    [_]
    (* num-components bytes-per-component)))


(def ip4-codec
  (->IPAddressCodec 4 10 "." 4 1))


(def ip6-codec
  (->IPAddressCodec 6 16 ":" 8 2))


(defn- min-bytes-for-uint
  [n]
  (loop [bytes-shift 1]
    (let [shifted (unsigned-bit-shift-right n (* 8 bytes-shift))]
      (if (zero? shifted)
        bytes-shift
        (recur (inc bytes-shift))))))


(defrecord UnsignedNumTranscoder
  [radix num-bytes]

  Codec

  (str->bytes
    [_ value]
    (let [dst (b/byte-array num-bytes)
          n (str->int value)]
      (when (> (min-bytes-for-uint n) num-bytes)
        (throw (ex-info (str "Value too big for " num-bytes " bytes")
                        {:value value :num-bytes num-bytes})))
      (write-uint-bytes! n dst 0 num-bytes)
      dst))


  (bytes->str
    [_ data]
    (when (not= (alength ^bytes data) num-bytes)
      (throw (ex-info "Incorrect number of bytes"
                      {:num-bytes-given (alength ^bytes data)
                       :expected-bytes num-bytes})))
    (-> data big-endian-bytes->uint (int->str radix)))


  (fixed-byte-length
    [_]
    num-bytes))


(def ushort-codec
  (->UnsignedNumTranscoder 10 2))


(def no-value-codec
  (reify Codec
    (str->bytes
      [_ value]
      (when-not (nil? value)
        (throw (ex-info "Expected nil value for no value protocol"
                        {:value value})))
      (b/byte-array 0))

    (bytes->str
      [_ data]
      (when (pos? (alength ^bytes data))
        (throw (ex-info "Expected 0 length array for no value protocol"
                        {:data (seq data)}))))

    (fixed-byte-length [_] 0)))


(def utf8-codec
  (reify Codec
    (str->bytes
      [_ value]
      #?(:clj  (-> ^String value (.getBytes "UTF8"))
         :cljs (-> ^string value crypt/stringToByteArray js/Uint8Array.)))

    (bytes->str
      [_ data]
      #?(:clj  (String. ^bytes data "UTF8")
         :cljs (crypt/byteArrayToString data)))

    (fixed-byte-length
      [_]
      nil)))
