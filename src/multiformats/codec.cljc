(ns multiformats.codec
  "Multicodec is a multiformat which wraps other formats with a tiny bit of
  self-description. A multicodec identifier may either be a varint (in a byte
  string) or a character (in a text string).

  https://github.com/multiformats/multicodec"
  (:require
    [multiformats.address :as maddr]
    [multiformats.base :as mbase]
    [multiformats.hash :as mhash]))


;; ## Code Tables

(def miscellaneous-codes
  "Miscellaneous codes."
  {:raw 0x55  ; Raw binary data.
   ,,,})


(def serialization-codes
  "General-purpose serialization formats."
  {:cbor     0x51    ; CBOR
   ;:bson    0x??    ; Binary JSON
   ;:ubjson  0x??    ; Universal Binary JSON
   :protobuf 0x50    ; Protocol Buffers
   ;:capnp   0x??    ; Cap-n-Proto
   ;:flatbuf 0x??    ; FlatBuffers
   :rlp      0x60    ; Recursive Length Prefix
   ;:msgpack 0x??    ; MessagePack
   ;:binc    0x??    ; Binc
   :bencode  0x63    ; Bencode
   ,,,})


(def multiformat-codes
  "Generic codes to indicate which multiformat a value represents."
  {:multicodec 0x30
   :multihash  0x31
   :multiaddr  0x32
   :multibase  0x33})


(def ipld-codes
  "Structured data formats used in IPLD and other systems."
  {:ipld-pb              0x70    ; MerkleDAG protobuf
   :ipld-cbor            0x71    ; MerkleDAG cbor
   :ipld-json            0x0129  ; MerkleDAG json
   :git-raw              0x78    ; Raw Git object
   :eth-block            0x90    ; Ethereum Block (RLP)
   :eth-block-list       0x91    ; Ethereum Block List (RLP)
   :eth-tx-trie          0x92    ; Ethereum Transaction Trie (Eth-Trie)
   :eth-tx               0x93    ; Ethereum Transaction (RLP)
   :eth-tx-receipt-trie  0x94    ; Ethereum Transaction Receipt Trie (Eth-Trie)
   :eth-tx-receipt       0x95    ; Ethereum Transaction Receipt (RLP)
   :eth-state-trie       0x96    ; Ethereum State Trie (Eth-Secure-Trie)
   :eth-account-snapshot 0x97    ; Ethereum Account Snapshot (RLP)
   :eth-storage-trie     0x98    ; Ethereum Contract Storage Trie (Eth-Secure-Trie)
   :bitcoin-block        0xb0    ; Bitcoin Block
   :bitcoin-tx           0xb1    ; Bitcoin Tx
   :zcash-block          0xc0    ; Zcash Block
   :zcash-tx             0xc1    ; Zcash Tx
   :stellar-block        0xd0    ; Stellar Block
   :stellar-tx           0xd1    ; Stellar Tx
   :decred-block         0xe0    ; Decred Block
   :decred-tx            0xe1    ; Decred Tx
   :dash-block           0xf0    ; Dash Block
   :dash-tx              0xf1    ; Dash Tx
   :torrent-info         0x7b    ; Torrent file info field (bencoded)
   :torrent-file         0x7c    ; Torrent file (bencoded)
   :ed25519-pub          0xed    ; Ed25519 public key
   ,,,})



;; ## Lookup Maps

(def key->code
  "Map of codec keys to compact code values."
  ; TODO: check for conflicts
  (merge
    miscellaneous-codes
    multiformat-codes
    maddr/codes
    mbase/codes
    mhash/codes
    serialization-codes
    ipld-codes))


(def code->key
  "Map of compact code values to codec keys."
  (into {} (map (juxt val key)) key->code))


; TODO: function to register new codes?
