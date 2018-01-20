(require 'evil-lispy)

(defun elisp-setup ()
  (lispy-mode)
  (evil-lispy-mode))

(add-hook 'emacs-lisp-mode-hook #'elisp-setup)
