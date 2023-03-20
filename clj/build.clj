(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.rusher/scimacs)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def uber-file "target/uber.jar")
(def basis (b/create-basis {:project "deps.edn" :aliases [:native :native-22.3.1]}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn libscimacs [_]
  (clean nil)
  (println "Compiling clojure...")
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                             "-Dclojure.spec.skip-macros=true"]
                  :ns-compile '[com.rusher.scimacs]})

  (println "Compiling java...")
  (b/javac {:src-dirs ["src"]
            :class-dir class-dir
            :basis basis})

  (println "Creating uberjar:" uber-file)
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis}))
