(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'elpy-mode-hook
	  (lambda ()
	    (paredit-mode)
	    (rainbow-delimiter-mode)
	    ))


;;(setq py-autopep8-options '("--max-line-length=100"))
