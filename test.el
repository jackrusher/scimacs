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


(defun my-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(scimacs-eval-sci
 (my-file-contents "transpiler.clj"))

(defun clj-str-fn (body)
  (scimacs-eval-sci (format "(transpiler/transpile '%s)" body)))

(defmacro clj! (body)
  (let* ((elisp (clj-str-fn body))
         (evaled (eval (car (read-from-string elisp)))))
    `(quote ,evaled)))

(clj!
 (defn foo [x]
   (let [y (inc x)]
     (* y 2))))

(clj!
 (defn foo [x & xs]
   (let [y (inc x)]
     (* y 2))))

(clj!
 (comment
  (foo 1)
  (foo 1 2)
  (foo 1 2 3)))

(setq my-foo (foo 10))
my-foo ;; 22

(clj!
 (let [x 1]
   (setq x (inc x))
   (inc x)))

(clj! (map (fn [x] (* x x)) [1 2 3])) ;; => (1 4 9)

(clj! (defn inc [x]
        (+ x 1)))

(clj! (map #'inc [1 2 3]))

;; #' may be elided for higher order functions in map
(clj! (map inc [1 2 3]))

;;
