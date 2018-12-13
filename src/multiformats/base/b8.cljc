(ns multiformats.base.b8
  "Octal base encoding implementation."
  (:refer-clojure :exclude [format])
  (:require
    [alphabase.core :as abc]))


(def alphabet "01234567")


(defn format
  "Format byte data into an octal string."
  [data]
  (abc/encode alphabet data))


(defn parse
  "Parse an octal string into byte data."
  [string]
  (abc/decode alphabet string))
