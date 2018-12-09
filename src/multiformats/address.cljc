(ns multiformats.address
  "Multiaddr aims to make network addresses future-proof, composable, and
  efficient.

  https://github.com/multiformats/multiaddr"
  (:require
    [multiformats.varint :as varint]))


(def codes
  "Address identifiers for use in multiaddrs."
  {:ip4         0x04
   :ip6         0x29
   :ip6zone     0x2A
   :tcp         0x06
   :udp         0x0111
   :dccp        0x21
   :sctp        0x84
   :udt         0x012D
   :utp         0x012E
   :ipfs        0x01A5
   :http        0x01E0
   :https       0x01BB
   :quic        0x01CC
   :ws          0x01DD
   :onion       0x01BC
   :p2p-circuit 0x0122
   :dns4        0x36
   :dns6        0x37
   :dnsaddr     0x38
   ,,,})


