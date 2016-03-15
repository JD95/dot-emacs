(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 ;;'(haskell-process-type 'stack-ghci)
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (tango-dark)))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; External Scripts
(setq launch_scripts '("editor.el"
		       "haskell.el"
		       "org_mode.el"))

(setq f (lambda (file_name)
	  (load-file (concat "~/emacs_launch_scripts/" file_name))))
(mapcar f launch_scripts)


