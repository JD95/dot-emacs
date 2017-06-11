;; Eval Region
(global-set-key (kbd "C-c C-l") 'eval-last-sexp)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)))
