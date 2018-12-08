(ns multiformats.base.b58
  "Base 58 encoding implementation variants."
  (:require
    [alphabase.core :as abc]))


(def btc-alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")


(defn formatter
  "Construct a function to format byte data as base58."
  [alphabet]
  (fn format
    [data]
    (abc/encode alphabet data)))


(defn parser
  "Construct a function to parse base58-encoded string as byte data."
  [alphabet]
  (fn parse
    [string]
    (abc/decode alphabet string)))
