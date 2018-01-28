;;; lisp.el --- Utilities for Emacs Lisp Mode

;;; Commentary:
;; 

;;; Code:
(require 'evil-lispy)
(defun lisp-setup ()
  (lispy-mode)
  (rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map "M-s" 'avy-goto-word-1)
  (evil-lispy-mode)
  )

(add-hook 'emacs-lisp-mode-hook #'lisp-setup)
(global-set-key (kbd "C-c C-l") 'eval-last-sexp)

(provide 'lisp)

;;; lisp.el ends here
