(ns multiformats.varint
  "Implementation of an MSB unsigned variable-size integer.

  Unsigned integers are serialized 7 bits at a time, starting with the
  least-significant bits. The highest bit (msb) in each output byte indicates
  if there is a continuation byte.

  https://github.com/multiformats/unsigned-varint"
  (:require
    [alphabase.bytes :as b]))


;; ## Encoding

(defn write-bytes
  "Write a value as a varint to a byte array at the given offset. Returns the
  number of bytes written."
  [value ^bytes buffer offset]
  (when (neg? value)
    (throw (ex-info "Varints are unsigned and cannot be negative"
                    {:value value})))
  (loop [v value
         i 0]
    ; Check for index out of bounds.
    (when (<= (alength buffer) (+ offset i))
      (throw (ex-info
               (str "Varint write index out of bounds at position "
                    (+ offset i) " (" i " bytes from offset " offset ")")
               {:offset (+ offset i)})))
    ; Check for overflow.
    (when (<= 9 i)
      (throw (ex-info
               "Varints larger than nine bytes are not currently supported"
               {:value value})))
    (if (<= 0x80 v)
      ; Write continuation byte and recur.
      (let [b (bit-or (bit-and v 0x7F) 0x80)]
        (b/set-byte buffer (+ offset i) b)
        (recur (unsigned-bit-shift-right v 7)
               (inc i)))
      ; Final byte.
      (let [b (bit-and v 0x7F)]
        (b/set-byte buffer (+ offset i) b)
        (inc i)))))


(defn encode
  "Encode a value as a sequence of varint bytes. Returns the encoded byte
  array."
  ^bytes
  [value]
  (let [buffer (b/byte-array 9)
        length (write-bytes value buffer 0)
        result (b/byte-array length)]
    (b/copy buffer 0 result 0 length)
    result))



;; ## Decoding

(defn read-bytes
  "Read bytes from the byte array at the given offset. Returns a tuple with the
  decoded varint and the number of bytes read."
  [^bytes data offset]
  (loop [i offset
         n 0
         v 0]
    ; Check for index out of bounds.
    (when (<= (alength data) i)
      (throw (ex-info
               (str "Ran out of bytes to decode at position " i
                    " (" n " bytes from offset " offset ")")
               {:offset offset
                :length (alength data)})))
    ; Check for overflow of soft limit.
    (when (<= 9 n)
      (throw (ex-info
               "Varints larger than nine bytes are not currently supported"
               {:offset offset})))
    ; Decode next byte.
    (let [b (b/get-byte data i)]
      (if (< b 0x80)
        ; Final byte.
        [(bit-or (bit-shift-left b (* 7 n)) v)
         (inc n)]
        ; Continuation byte. Add masked lower bits and recur.
        (recur (inc i)
               (inc n)
               (bit-or (bit-shift-left (bit-and b 0x7F) (* 7 n)) v))))))


(defn decode
  "Decode a byte array as a varint value. Returns the decoded value.

  This is a shorthand for reading the bytes at the beginning of the array and
  ignoring any extra data."
  [buffer]
  (first (read-bytes buffer 0)))
