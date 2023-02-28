(ns build
  "Build instructions for clj-multiformats."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as d]))


(def lib-name 'mvxcvi/multiformats)
(def version (str "0.3." (b/git-count-revs nil)))

(def src-dir "src")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib-name) version))

(def basis (b/create-basis {:project "deps.edn"}))


(defn clean
  "Remove compiled artifacts."
  [_]
  (b/delete {:path "target"}))


(defn pom
  "Write out a pom.xml file for the project."
  [_]
  (let [commit-sha (b/git-process {:git-args "rev-parse HEAD"})]
    (b/write-pom
      {:basis basis
       :lib lib-name
       :version version
       :src-pom "doc/pom.xml"
       :src-dirs [src-dir]
       :class-dir class-dir
       :scm {:tag commit-sha}})))


(defn jar
  "Build a JAR file for distribution."
  [_]
  (pom nil)
  (b/copy-dir
    {:src-dirs [src-dir]
     :target-dir class-dir})
  (b/jar
    {:class-dir class-dir
     :jar-file jar-file}))


(defn install
  "Install a JAR into the local Maven repository."
  [_]
  (when-not (.exists (io/file jar-file))
    (jar nil))
  (b/install
    {:basis basis
     :lib lib-name
     :version version
     :jar-file jar-file
     :class-dir class-dir}))


(defn deploy
  "Deploy the JAR to Clojars."
  [opts]
  (clean opts)
  (jar opts)
  (let [pom-file (b/pom-path
                   {:class-dir class-dir
                    :lib lib-name})]
    (d/deploy
      (assoc opts
             :installer :remote
             :sign-releases? true
             :pom-file pom-file
             :artifact jar-file))))
