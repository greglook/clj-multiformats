(ns multiformats.codec
  "Multicodec is a multiformat which wraps other formats with a tiny bit of
  self-description. A multicodec identifier may either be a varint (in a byte
  string) or a symbol (in a text string).

  https://github.com/multiformats/multicodec")


(def table
  "Map of codec keys to their compact code values."
  ; TODO: fill out more codes?
  {:raw 0x55

   ; General Data Formats
   :protobuf 0x50
   :cbor     0x51
   :bencode  0x63

   ; Multiformats
   :multicodec 0x30
   :multihash  0x31
   :multiaddr  0x32

   ; IPLD
   :dag-pb   0x70
   :dag-cbor 0x71

   ; Git
   :git-raw 0x78})


; TODO: register new codec codes?


(defn find-codec
  "Looks up a codec by keyword name or code number. Returns a map with the
  codec's `:name` and `:code`, or `nil` if the value does not map to any valid
  multicodec."
  [k]
  (cond
    (keyword? k)
    (when-let [code (get table k)]
      {:code code, :name k})

    (not (integer? k))
    nil

    :else
    (some #(when (= k (val %))
             {:code k, :name (key %)})
          table)))
