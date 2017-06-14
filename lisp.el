;;; lisp.el --- Utilities for Emacs Lisp Mode

;;; Commentary:
;; 

;;; Code:

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (paredit-mode)
	    ))

(provide 'lisp)

;;; lisp.el ends here
