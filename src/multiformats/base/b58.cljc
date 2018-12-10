(ns multiformats.base.b58
  "Base 58 encoding implementation variants."
  (:require
    [alphabase.core :as abc]))


(def btc-alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")


(defn format-btc
  "Format byte data as base58 using the bitcoin alphabet."
  [data]
  (abc/encode btc-alphabet data))


(defn parse-btc
  "Parse a base58-encoded string as byte data using the bitcoin alphabet."
  [string]
  (abc/decode btc-alphabet string))
