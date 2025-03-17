(ns multiformats.address-test
  (:require
    [alphabase.bytes :as b]
    [clojure.test :refer [deftest testing is]]
    [multiformats.address :as address]))


(deftest creation
  (testing "of empty address"
    (let [addr (address/create)]
      (is (address/address? addr)
          "should create a new address")
      (is (empty? addr))
      (is (zero? (count addr)))
      (is (= "" (str addr))
          "should represent as an empty string")))
  (testing "with basic protocols"
    (let [addr (address/create [[:dns "mvxcvi.com"] [:tcp 80]])]
      (is (address/address? addr))
      (is (= "/dns/mvxcvi.com/tcp/80" (str addr))))))


(deftest predicate
  (is (address/address? (address/create [[:tcp 80]])))
  (is (not (address/address? nil)))
  (is (not (address/address? "foo"))))


(deftest protocol-encoding
  (testing "with bad entry"
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Address entry must be a protocol keyword or vector pair"
          (address/create [123]))
        "should throw when given an invalid entry type")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Address entry must be a protocol keyword or vector pair"
          (address/create [["foo" 123]]))
        "should throw when given an invalid protocol type")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Address entry must be a protocol keyword or vector pair"
          (address/create [[:dns "example.com" 123]]))
        "should throw when given an entry which is too long")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Unsupported protocol type"
          (address/create [[:not-valid 123]]))
        "should throw on unsupported protocol keyword"))
  (testing "for null values"
    (is (= [[:tls]]
           (seq (address/create [:tls]))))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol tls does not support values"
          (address/create [[:tls "abc"]]))
        "should throw when null protocols given a value"))
  (testing "for utf8 values"
    (is (= [[:dns "example.com"]]
           (seq (address/create [[:dns "example.com"]]))))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol dns requires a UTF-8 string value"
          (address/create [[:dns 123]]))
        "should throw when given a non-string value"))
  (testing "for ushort values"
    (is (= [[:tcp 80]]
           (seq (address/create [[:tcp 80]]))))
    (is (= [[:tcp 80]]
           (seq (address/create [[:tcp "80"]])))
        "should parse string inputs")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol tcp has invalid string value"
          (address/create [[:tcp "xyz"]]))
        "should throw when given a non-numeric string value")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol tcp requires an unsigned short value or numeric string"
          (address/create [[:tcp :foo]]))
        "should throw when given an invalid value type")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol tcp has value too big for unsigned short"
          (address/create [[:tcp 66000]]))
        "should throw when given an out-of-range number"))
  (testing "for ip4 values"
    (is (= [[:ip4 "127.0.0.1"]]
           (seq (address/create [[:ip4 "127.0.0.1"]]))))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Invalid IPv4 address string"
          (address/create [[:ip4 "1.2.3.256"]]))
        "should throw on invalid ip4 address")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Invalid IPv4 address string"
          (address/create [[:ip4 "::1"]]))
        "should throw on invalid ip4 address")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol ip4 requires an IP address string"
          (address/create [[:ip4 123]]))
        "should throw on invalid value types"))
  (testing "for ip6 values"
    (is (= [[:ip6 "0:0:0:0:0:0:0:1"]]
           (seq (address/create [[:ip6 "::1"]])))
        "should expand :: placeholder")
    (is (= [[:ip6 "2001:db8:85a3:0:0:8a2e:370:7334"]]
           (seq (address/create [[:ip6 "2001:0db8:85a3:0000:0000:8a2e:0370:7334"]]))))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Invalid IPv6 address string"
          (address/create [[:ip6 "1.2.3.4"]]))
        "should throw on invalid ip6 address")
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Protocol ip6 requires an IP address string"
          (address/create [[:ip6 123]]))
        "should throw on invalid value types")))


(deftest string-serialization
  (is (= "/ip4/127.0.0.1" (str (address/parse "/ip4/127.0.0.1"))))
  (is (= "/ip4/10.8.0.5/tcp/80" (str (address/parse "/ip4/10.8.0.5/tcp/80"))))
  (is (= "/dns/example.com/udp/9090/quic" (str (address/parse "/dns/example.com/udp/9090/quic"))))
  (is (= "/dns6/abc.xyz/tcp/3217/tls/ws" (str (address/parse "/dns6/abc.xyz/tcp/3217/tls/ws")))))


(deftest binary-serialization
  (let [addr (address/create [[:ip4 "127.0.0.1"]])
        data [4 127 0 0 1]]
    (is (= data (b/byte-seq (address/encode addr))))
    (is (= addr (address/decode (b/init-bytes data)))))
  (let [addr (address/create [[:ip4 "10.8.0.5"] [:tcp 80]])
        data [4, 10 8 0 5
              6, 0 80]]
    (is (= data (b/byte-seq (address/encode addr))))
    (is (= addr (address/decode (b/init-bytes data)))))
  (let [addr (address/create [[:ip6 "2001:db8:85a3:0:0:8a2e:370:7334"]
                              [:sctp 8080]
                              [:tls]])
        data [41, 0x20 0x01 0x0d 0xb8 0x85 0xa3 0x00 0x00 0x00 0x00 0x8a 0x2e 0x03 0x70 0x73 0x34
              132 1, 31 144
              192 3]]
    (is (= data (b/byte-seq (address/encode addr))))
    (is (= addr (address/decode (b/init-bytes data)))))
  (let [addr (address/create [[:dns "example.com"]
                              [:udp 9090]
                              :quic])
        data [53, 11, 101 120 97 109 112 108 101 46 99 111 109
              145 2, 35 130
              204 3]]
    (is (= data (b/byte-seq (address/encode addr))))
    (is (= addr (address/decode (b/init-bytes data))))))


(deftest value-equality
  (let [addr (address/create [[:ip4 "127.0.0.1"]])]
    (is (= addr addr))
    (is (= addr (address/parse "/ip4/127.0.0.1")))
    (is (not= addr (address/create [[:ip4 "127.0.0.2"]])))
    (is (not= addr (address/create [[:ip4 "127.0.0.1"] [:tcp 80]])))
    (is (not= addr nil))
    (is (not= addr :abc))))


(deftest value-hashing
  (let [addr (address/create [[:dns "example.com"] [:tcp 443] [:tls] [:http]])]
    (is (= (hash addr) (hash addr)))
    (is (= (hash addr) (hash (address/parse "/dns/example.com/tcp/443/tls/http"))))
    (is (not= (hash addr) (hash (address/create [[:dns "example.com"]]))))))


(deftest value-comparison
  (let [localhost (address/create [[:ip4 "127.0.0.1"]])
        example-com (address/create [[:dns "example.com"] [:tcp 443] [:tls] [:http]])]
    (is (zero? (compare localhost localhost)))
    (is (zero? (compare localhost (address/parse "/ip4/127.0.0.1"))))
    (is (neg? (compare localhost example-com)))
    (is (pos? (compare example-com localhost)))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Cannot compare multiaddress value to"
          (compare localhost "abc"))
        "should throw on invalid comparison")))


(deftest value-metadata
  (let [addr (address/create [[:ip4 "127.0.0.1"]])]
    (is (nil? (meta addr))
        "should be absent on creation")
    (is (= {:abc true} (meta (with-meta addr {:abc true})))
        "should support metadata")
    (is (= addr (with-meta addr {:abc true}))
        "should not affect equality")))


(deftest entry-sequence
  (let [addr (address/create [[:dns "example.com"] [:tcp 443] [:tls] [:http]])]
    (is (= 4 (count addr)))
    (is (= [[:dns "example.com"] [:tcp 443] [:tls] [:http]] (seq addr)))
    (is (= [:dns "example.com"] (nth addr 0)))
    (is (= [:tls] (nth addr 2)))
    (is (= :missing (nth addr 4 :missing)))
    (is (thrown-with-msg? #?(:clj IndexOutOfBoundsException, :cljs js/Error) #"Index 4 is outside the 4 elements in the address"
          (nth addr 4))
        "should throw on out-of-bounds without not-found")))


(deftest entry-manipulation
  (let [addr (address/create [[:dns "example.com"] [:tcp 443]])]
    (is (= (address/create) (empty addr))
        "should be emptyable")
    (is (= (address/create [[:dns "example.com"] [:tcp 443] :tls])
           (conj addr :tls)))
    (is (= (address/create [[:dns "example.com"] [:tcp 443] :tls :http])
           (conj addr [:tls nil] [:http])))
    (is (= [:tcp 443] (peek addr)))
    (is (= (address/create [[:dns "example.com"]])
           (pop addr)))
    (is (= (address/create) (pop (pop addr))))
    (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #"Can't pop empty address"
          (pop (address/create))))))
