;;; purescript.el --- Configurations for purescript files

;;; Commentary:
;; 

(require 'psc-ide)

;;; Code:

(add-hook 'purescript-mode-hook
	  (lambda ()
	    (psc-ide-mode)
	    (company-mode)
	    (flycheck-mode)
	    (turn-on-purescript-indentation)
	    ))

(provide 'purescript)

;;; purescript.el ends here
