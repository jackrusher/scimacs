(ns com.rusher.scimacs
  (:require [sci.core :as sci])
  (:gen-class :methods [^{:static true} [evalString [String] String]]))

(set! *warn-on-reflection* true)

(sci/alter-var-root sci/out (constantly *out*)) ;; enable println, etc.
(sci/alter-var-root sci/err (constantly *err*)) ;; enable println, etc.

(def ctx (sci/init {:namespaces {}
                    :classes {}}))

(defn -evalString [s]
  (try (pr-str (sci/eval-string* ctx s))
       (catch Exception e
         (pr-str {:error (str (type e))
                  :message (.getMessage e)}))))

;; as second arg to eval-string, this binds a cheshire.core function within sci
#_{:namespaces {'cheshire.core {'generate-string cheshire/generate-string}}}
