;;; purescript.el --- Configurations for purescript files

;;; Commentary:
;; 

(require 'psc-ide)

;;; Code:

(defun purescript-setup ()
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode)
  (turn-on-purescript-indentation)
  )

(add-hook 'purescript-mode-hook #'purescript-setup)

(provide 'purescript)

;;; purescript.el ends here
