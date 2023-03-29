#!/opt/homebrew/bin/emacs-28.2 --script

(message "Testing scimacs...")

;; tell emacs to look in current directory for loadable modules
(add-to-list 'load-path ".")

;; try to find the loadable module
(if-let ((location (locate-library "scimacs")))
    (progn
      (message (format "Loadable module found: %s" location))
      (require 'scimacs)
      (message "Scimacs version %s loaded with these symbols:" scimacs-version)
      (mapatoms (lambda (symbol)
                  (let ((symbol-name (symbol-name symbol)))
                    (if (string-prefix-p "scimacs-" symbol-name)
                        (message (format "  %s" symbol-name))))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; too simple, but a start
      
      (add-to-list 'load-path "/Users/jack/.emacs.d/elpa/parseedn-20220207.1352/")
      (add-to-list 'load-path "/Users/jack/.emacs.d/elpa/parseclj-20220313.1649")
      
      (require 'parseedn)

      (defun scimacs-emacs-apply (fn-string params-string)
        (condition-case err
            (let ((fn-form (car (read-from-string fn-string)))
                  (params-form (car (read-from-string params-string))))
              (parseedn-print-str
               (if (eq params-form '()) 
                   (funcall fn-form)
                 (apply fn-form params-form))))
          (error (error-message-string err))))
      
      (message "Evaluating with scimacs-eval-string:")
      (mapcar (lambda (form)
                (message (format "  %s ; => %s" form (scimacs-eval-string form))))
              '("[1 2 3]"
                "(+ 1 1)"
                "(reduce + (range 5))"))

      (defmacro scimacs-eval (clj-form)
        (interactive)
        (parseedn-read-str (scimacs-eval-string (prin1-to-string clj-form))))
      
      (message "Evaluating with scimacs-eval:")
      (message "  %s ;=> %s" '(list 3 2 1) (scimacs-eval '(list 3 2 1)))
      (message "  %s ;=> %s" [1 1 2 3 5 8] (scimacs-eval [1 1 2 3 5 8]))
      (message "  %s ;=> %s" '(mapv inc (range 5)) (scimacs-eval (mapv inc (range 5))))
      ;; made sure def'd vars are visible in future invocations
      (message "  %s ;=> %s" '(defn foo [x] (inc x)) (scimacs-eval (defn foo [x] (inc x))))
      (message "  %s ;=> %s" '(foo 1) (scimacs-eval (foo 1)))
      ;; apply this emacs function to this argument
      (message "  %s ;=> %s" "(emacs/call \"message\" \"%s %s\" \"quo\" \"vadimus?\")"
               (scimacs-eval (emacs/call "message" "%s %s" "quo" "vadimus?")))
      (message "  %s ;=> %s" "(emacs/call \"+\" 1 1)"
         (scimacs-eval (emacs/call "+" 1 1))))
  (message "Could not find loadable module!"))

;; TODO

;; - need to fix the parsedn's printer to print buffers as data literals,

;; - add defrecord(s) for buffers and other emacs special types on the CLJ side w/ reading and printing

;; - error data literal to send emacs errors back to sci, which will then forward them back to emacs.
