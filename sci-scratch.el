;; A small example of setting up a persistent scratch buffer in which
;; clojure forms are evaluated using sci.

;; add the directory containing the shared object to your load path
;; XXX (change to wherever you compiled scimacs)
(add-to-list 'load-path "/Users/jack/src/scimacs/")

;; eval this to make sure emacs can find the library
(locate-library "scimacs")

;; this require will load the external module
(require 'scimacs)

;; evaluate a form in sci and display the result
(defun eval-sci (clj-str)
  (interactive)
  (message (scimacs-eval-sci clj-str)))

;; Evaluate the last sexp in sci and display the result. Depends on
;; cider's last-sexp function.
(defun sci-eval-last-sexp ()
  "Evaluate the expression preceding point using sci."
  (interactive)
  (eval-sci (apply #'buffer-substring-no-properties (cider-last-sexp 'bounds))))

;; Evaluate the last sexp in sci and display the result. Depends on
;; cider's defun-at-point function.
(defun sci-eval-defun ()
  "Evaluate the expression preceding point using sci."
  (interactive)
  (eval-sci (apply #'buffer-substring-no-properties (cider-defun-at-point 'bounds))))

(defun sci-scratch ()
  "Switch to the *sci-scratch* buffer (creating it and setting the right modes if needed)."
  (interactive)
  (let ((buf (get-buffer-create "*sci-scratch*"))) 
    (with-current-buffer buf
      (clojure-mode)
      (use-local-map (copy-keymap clojure-mode-map))
      (local-set-key (kbd "<s-return>") 'sci-eval-last-sexp)
      (local-set-key (kbd "<S-s-return>") 'sci-eval-defun))
    (switch-to-buffer buf)))
