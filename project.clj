(defproject mvxcvi/multiformats "0.1.0-SNAPSHOT"
  :description "Native Clojure implementation of the multiformat standards."
  :url "https://github.com/multiformats/clj-multiformats"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :aliases
  {"clj:test" ["do" "check" "test"]
   "cljs:check" ["with-profile" "+doo" "cljsbuild" "once"]
   "cljs:repl" ["run" "-m" "clojure.main" "dev/cljs_repl.clj"]
   "cljs:test" ["doo" "phantom" "test" "once"]}

  :plugins
  [[lein-cljsbuild "1.1.7"
    :exclusions [org.clojure/clojure]]
   [lein-doo "0.1.11"
    :exclusions [org.clojure/clojure
                 org.clojure/clojurescript]]
   [lein-cloverage "1.0.13"
    :exclusions [org.clojure/clojure]]
   ; Conflict resolution ಠ_ಠ
   [meta-merge "1.0.0"]]

  :dependencies
  [[mvxcvi/alphabase "1.0.0"]]

  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:optimizations :whitespace
                        :output-dir "target/cljs/out"
                        :output-to "target/cljs/tests.js"
                        :main multiformats.test-runner}}]}

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/clojure "1.9.0"]
     [org.clojure/clojurescript "1.10.439"]
     [org.clojure/tools.namespace "0.2.11"]
     ; Conflict resolution ಠ_ಠ
     [com.google.code.findbugs/jsr305 "3.0.2"]
     [com.google.errorprone/error_prone_annotations "2.1.3"]]}

   :doo
   {:dependencies
    [[doo "0.1.11"]]}

   :repl
   {:source-paths ["dev"]
    :whidbey
    {:tag-types {'multiformats.hash.Multihash {'multi/hash 'str}}}}})
