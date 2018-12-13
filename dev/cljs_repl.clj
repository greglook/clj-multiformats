(require 'cljs.repl)
;(require 'cljs.repl.rhino)
(require 'cljs.repl.node)

; TODO: figure out how to enable TypedArrays in newer (1.7.7.1) versions of rhino. See:
; https://github.com/mozilla/rhino/blob/72933223618985acd854dfc604d93822daca4ad2/src/org/mozilla/javascript/ScriptRuntime.java#L221
; https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/repl/rhino.clj

(cljs.repl/repl
  ;(cljs.repl.rhino/repl-env)
  (cljs.repl.node/repl-env)
  :watch "src"
  :output-dir "target/cljs")
