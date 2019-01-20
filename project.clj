(defproject mvxcvi/multiformats "0.1.1"
  :description "Native Clojure implementation of the multiformat standards."
  :url "https://github.com/multiformats/clj-multiformats"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :aliases
  {"clj:test" ["do" ["check"] ["test"]]
   "cljs:check" ["with-profile" "+doo" "cljsbuild" "once"]
   "cljs:repl" ["run" "-m" "clojure.main" "dev/cljs_repl.clj"]
   "cljs:test" ["doo" "rhino" "test" "once"]}

  :plugins
  [[lein-cljsbuild "1.1.7"
    :exclusions [org.clojure/clojure]]
   [lein-doo "0.1.11"
    :exclusions [org.clojure/clojure
                 org.clojure/clojurescript]]
   [lein-cloverage "1.0.13"
    :exclusions [org.clojure/clojure
                 venantius/ultra]]
   ; Conflict resolution ಠ_ಠ
   [meta-merge "1.0.0"]]

  :dependencies
  [[mvxcvi/alphabase "2.0.3"]
   [commons-codec "1.11"]]

  :hiera
  {:cluster-depth 2
   ;:vertical false
   :show-external false}

  :codox
  {:metadata {:doc/format :markdown}
   :source-uri "https://github.com/greglook/clj-multiformats/blob/master/{filepath}#L{line}"
   :output-path "target/doc/api"}

  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:optimizations :whitespace
                        :output-dir "target/cljs/out"
                        :output-to "target/cljs/tests.js"
                        :main multiformats.test-runner}}]}

  :whidbey
  {:tag-types {'multiformats.hash.Multihash {'multi/hash str}
               'multiformats.cid.ContentID {'multi/cid str}}}

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/clojure "1.10.0"]
     [org.clojure/clojurescript "1.10.439"]
     [org.clojure/tools.namespace "0.2.11"]
     ; Conflict resolution ಠ_ಠ
     [com.google.code.findbugs/jsr305 "3.0.2"]
     [com.google.errorprone/error_prone_annotations "2.3.2"]
     [org.clojure/tools.reader "1.3.2"]]}

   :doo
   {:dependencies
    [[doo "0.1.11"]]}

   :repl
   {:source-paths ["dev"]}})
