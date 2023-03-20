(ns com.rusher.scimacs
  (:require [sci.core :as sci])
  (:gen-class :methods [^{:static true} [evalString [String] String]]))

(defn -evalString [s]
  (sci/binding [sci/out *out*] ; enable println, etc.
    (str (try (sci/eval-string s)
              (catch Exception e
                {:error (str (type e))
                 :message (.getMessage e)})))))

;; as second arg to eval-string, this binds a cheshire.core function within sci
#_{:namespaces {'cheshire.core {'generate-string cheshire/generate-string}}}
