(defproject mvxcvi/multiformats "0.1.0-SNAPSHOT"
  :description "Native Clojure implementation of the multiformat standards."
  :url "https://github.com/multiformats/clj-multiformats"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :dependencies
  [[mvxcvi/alphabase "1.0.0"]]

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/clojure "1.9.0"]
     [org.clojure/tools.namespace "0.2.11"]]}

   :repl
   {:source-paths ["dev"]}

   :coverage
   {:plugins [[lein-cloverage "1.0.13"]]}})
