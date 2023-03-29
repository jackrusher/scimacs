(ns com.rusher.scimacs
  (:require [sci.core :as sci])
  (:gen-class :methods [^{:static true} [evalString [java.util.function.BiFunction String] String]]))

(set! *warn-on-reflection* true)

(sci/alter-var-root sci/out (constantly *out*)) ;; enable println, etc.
(sci/alter-var-root sci/err (constantly *err*)) ;; enable println, etc.

(def ctx (sci/init {:namespaces {}
                    :classes {}}))

(defn -evalString [^java.util.function.BiFunction apply-fn ^String form-string]
  (try
    (pr-str (sci/eval-string*
             ;; apply-fn closes over our native eval_in_emacs w/ an
             ;; ephemeral emacs env pointer, so must be reset on every
             ;; call. We might switch to passing the apply-fn as a
             ;; dynamic variable here.
             (sci/add-namespace! ctx 'emacs {'call (fn ^String [f & params]
                                                     (.apply apply-fn f (pr-str params)))})
             form-string))
    (catch Exception e
      (pr-str {:error (str (type e))
               :message (.getMessage e)}))))

;; TODO We need an error writer/reader convention so elisp can throw
;; when an error is sent over. This will also involve improving the
;; parseedn library on the emacs side.
