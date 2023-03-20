#!/opt/homebrew/bin/emacs-28.2 --script

(message "Testing scimacs...")

;; tell emacs to look in current directory for loadable modules
(add-to-list 'load-path ".")

;; try to find the loadable module
(if-let ((location (locate-library "scimacs")))
    (progn
      (message (format "Loadable module found: %s" location))
      (require 'scimacs)
      (message "Symbols loaded from scimacs:")
      (mapatoms (lambda (symbol)
                  (let ((symbol-name (symbol-name symbol)))
                    (if (string-prefix-p "scimacs-" symbol-name)
                        (message (format "  %s" symbol-name))))))
      (message "Evaluating forms with sci:")
      (mapcar (lambda (form)
                (message (format "  %s ; => %s" form (scimacs-eval-sci form))))
              '("[1 2 3]"
                "(+ 1 1)"
                "(mapv inc (range 5))"
                "(reduce + (range 5))")))
  (message "Could not find loadable module!"))

(scimacs-eval-sci "
(ns transpiler)
(declare transpile)

(defn transpile-defn [[_defn name args & body]]
 `(~'defun ~name ~(sequence args) ~@(map transpile body)))

(defn transpile-let [[_let bindings & body]]
 `(~'let* ~(sequence (map (fn [[binding expr]]
                         (list binding (transpile expr)))
                       (partition 2 bindings)))
    ~@(map transpile body)))

(defn transpile-inc [[_let expr]]
 `(~'+ 1 ~(transpile expr)))

(defn transpile-map [_map fn expr]
  `(~'mapcar ~(transpile fn) ~(transpile expr)))

(defn transpile [form]
  (if (seq? form)
    (case (first form)
      let (transpile-let form)
      inc (transpile-inc form)
      defn (transpile-defn form)
      map (transpile-map form)
      (sequence (map transpile form)))
    form))")

(defun clj-str-fn (body)
  (scimacs-eval-sci (concat transpiler (format " (transpiler/transpile '%s)" body))))

(defmacro clj! (body)
  (let* ((elisp (clj-str-fn body))
         (evaled (eval (car (read-from-string elisp)))))
    `(quote ,evaled)))

(clj!
 (defn foo [x]
   (let [y (inc x)]
     (* y 2))))

(setq my-foo (foo 10))
my-foo ;; 22

(clj!
 (let [x 1]
   (setq x (inc x))
   (inc x)))

(clj-str-fn '(filter x x))
(clj-str-fn '(map inc [1 2 3]))
(clj! (mapv inc [1 2 3]))



