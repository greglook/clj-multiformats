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
  [buffer offset value]
  (when (neg? value)
    (throw (ex-info "Varints are unsigned and cannot be negative"
                    {:value value})))
  (loop [v value
         i 0]
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
        size (write-bytes buffer 0 value)
        result (b/byte-array size)]
    (b/copy buffer 0 result 0 size)
    result))



;; ## Decoding

(defn read-bytes
  "Read bytes from the byte array at the given offset. Returns a tuple with the
  decoded varint and the number of bytes read."
  [^bytes data offset]
  (loop [i offset
         n 0
         v 0]
    (if (< i (count data))
      ; Decode next byte.
      (let [b (b/get-byte data i)]
        (if (< b 0x80)
          ; Final byte.
          [(bit-or (bit-shift-left b (* 7 n)) v)
           (inc n)]
          ; Continuation byte.
          (if (<= 9 n)
            ; Check for overflow of soft limit.
            (throw (ex-info
                     "Varints greater than nine bytes are not currently supported"
                     {:offset offset}))
            ; Add masked lower bits and recur.
            (recur (inc i)
                   (inc n)
                   (bit-or (bit-shift-left (bit-and b 0x7F) (* 7 n)) v)))))
      ; Out of bytes to decode.
      (throw (ex-info
               (format "Ran out of bytes to decode at position %d (%d bytes from offset %d)"
                       i n offset)
               {:offset offset
                :length (count data)})))))


(defn decode
  "Decode a byte array as a varint value. Returns the decoded value.

  This is a shorthand for reading the bytes at the beginning of the array and
  ignoring any extra data."
  [buffer]
  (first (read-bytes buffer 0)))
