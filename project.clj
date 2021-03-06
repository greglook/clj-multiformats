(defproject mvxcvi/multiformats "0.2.2-SNAPSHOT"
  :description "Native Clojure implementation of the multiformat standards."
  :url "https://github.com/multiformats/clj-multiformats"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :aliases
  {"cljs:repl" ["run" "-m" "clojure.main" "dev/multiformats/cljs_repl.clj"]
   "cljs:check" ["with-profile" "+doo" "cljsbuild" "once"]
   "cljs:test" ["doo" "phantom" "test" "once"]}

  :plugins
  [[lein-cljsbuild "1.1.7"
    :exclusions [org.clojure/clojure]]
   [lein-doo "0.1.11"
    :exclusions [org.clojure/clojure
                 org.clojure/clojurescript]]
   [lein-cloverage "1.1.2"
    :exclusions [org.clojure/clojure
                 venantius/ultra]]]

  :dependencies
  [[mvxcvi/alphabase "2.1.0"]
   [commons-codec "1.14"]]

  :hiera
  {:cluster-depth 2
   :show-external false}

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
    [[org.clojure/clojure "1.10.1"]
     [org.clojure/clojurescript "1.10.597"]
     ;; Conflict resolution
     [com.google.code.findbugs/jsr305 "3.0.2"]
     [com.google.errorprone/error_prone_annotations "2.3.4"]
     [org.clojure/tools.reader "1.3.2"]]}

   :repl
   {:source-paths ["dev"]
    :repl-options {:init-ns multiformats.repl}
    :dependencies
    [[clj-stacktrace "0.2.8"]
     [org.clojure/tools.namespace "1.0.0"]]}

   :doo
   {:dependencies
    [[doo "0.1.11"]]}})
