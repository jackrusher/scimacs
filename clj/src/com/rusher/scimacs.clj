(ns com.rusher.scimacs
  (:require [sci.core :as sci])
  (:gen-class :methods [^{:static true} [evalString [java.util.function.BiConsumer String] String]]))

(set! *warn-on-reflection* true)

(sci/alter-var-root sci/out (constantly *out*)) ;; enable println, etc.
(sci/alter-var-root sci/err (constantly *err*)) ;; enable println, etc.

(def ctx (sci/init {:namespaces {}
                    :classes {}}))

;; TODO BiConsumer -> BiFunction when we add return value
(defn -evalString [^java.util.function.BiConsumer apply-fn ^String form-string]
  (try
    ;; should
    (pr-str (sci/eval-string*
             ;; apply-fn closes over our native eval_in_emacs w/ an
             ;; ephemeral emacs env pointer, so must be reset on every
             ;; call.
             ;; TODO currently only sends one param, should be a vector of them
             (sci/add-namespace! ctx 'emacs {'apply #(.accept apply-fn %1 %2)})
             form-string))
    (catch Exception e
      (pr-str {:error (str (type e))
               :message (.getMessage e)}))))
