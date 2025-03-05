(ns multiformats.address-test
  (:require
    [alphabase.bytes :as b]
    [clojure.test :refer [deftest are]]
    [multiformats.address :as address]))


(deftest parse-test
  (are [addr-str coll]
       (= (seq (address/parse addr-str)) coll)
    "/ip4/127.0.0.1" [[:ip4 "127.0.0.1"]]
    "/ip4/127.0.0.1/tcp/80" [[:ip4 "127.0.0.1"] [:tcp "80"]]
    "/ip4/127.0.0.1/udp/9090/quic" [[:ip4 "127.0.0.1"] [:udp "9090"] [:quic nil]]
    "/ip6/::1/tcp/3217" [[:ip6 "0:0:0:0:0:0:0:1"] [:tcp "3217"]]
    "/ip6/::1/tcp/3217/ws" [[:ip6 "0:0:0:0:0:0:0:1"] [:tcp "3217"] [:ws nil]]))


(deftest conj-test
  (are [base-addr entries expected-str]
       (= expected-str (str (into base-addr entries)))
    (address/parse "/ip4/127.0.0.1") [[:tcp "80"]] "/ip4/127.0.0.1/tcp/80"
    (address/create) [[:ip4 "127.0.0.1"] [:tcp "80"]] "/ip4/127.0.0.1/tcp/80"
    (address/parse "/ip4/127.0.0.1/udp/9090") [:quic] "/ip4/127.0.0.1/udp/9090/quic"))


(defn roundtrip
  [addr]
  (-> addr
      address/encode
      address/decode))


(deftest encode-decode-roundtrip
  (are [addr]
       (let [rt (roundtrip addr)]
         (and
           (b/bytes= (address/encode addr) (address/encode rt))
           (= addr (roundtrip addr))))
    (address/parse "/ip4/127.0.0.1")
    (address/parse "/ip6/::/tcp/0/wss")
    (address/parse "/ip4/1.2.3.4/tcp/3456/ws")
    (address/parse "/dnsaddr/protocol.ai/tcp/80")
    (address/create [:ip4 "127.0.0.1"] [:tcp "80"])
    (address/parse "/ip4/127.0.0.1/udp/9090")))
