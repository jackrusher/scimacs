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


