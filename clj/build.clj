(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.rusher/scimacs)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn libscimacs [_]
  (clean nil)
  
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                             "-Dclojure.spec.skip-macros=true"]})

  (b/javac {:src-dirs ["src"]
            :class-dir class-dir
            :basis basis
            ;; TODO fix classpath here to use GRAALVM's home rather than hardcoded MacOS path
            :javac-opts ["-cp" "/Library/Java/JavaVirtualMachines/graalvm-ce-java19-22.3.1/Contents/Home/lib/svm/builder/svm.jar:target/classes"]}))
