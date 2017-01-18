(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(org-agenda-files (quote ("d:/OneDrive/Planner/TODO.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; list the packages you want
(setq package-list '(avy))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
			 )
)

; activate all the packages (in particular autoloads)
(package-initialize)

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; External Config Scripts
(setq launch_scripts '("editor.el"
		       "org_mode.el"
		       "haskell.el"
		       "markdown.el"
		       "agda.el"))

(setq load_script (lambda (file_name)
	  (load-file (concat "~/emacs_launch_scripts/" file_name))))

(mapcar load_script launch_scripts)
