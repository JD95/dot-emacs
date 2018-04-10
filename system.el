(require 'cl-lib)

(require 'json)

;; Load from config file
(let* ((json-object-type 'hash-table)
       (json-array-type 'list)
       (json-key-type 'string)
       (json (json-read-file "~/emacs_launch_scripts/config.json")))
  (setq emacs-config json))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (gethash "agenda-files" emacs-config))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Setting the font
(set-face-attribute 'default nil :font "Fira Code")
(set-frame-font "Fira Code" nil t)

; list the packages you want
(setq package-list 
      ';; Themes
      (gotham-theme

       ;; Apps
       use-package
       
       ;; Navigation
       avy 
       smooth-scrolling
       evil
       evil-collection
       evil-magit
       evil-org
       evil-tutor

       ;; Research
       org-ref
       ox-reveal
       ox-twbs
       
       ;; Git
       magit
	
       ;; Text Manipulation
       drag-stuff

       ;; Windows
       powershell

       ;; General coding
       paredit
       rainbow-delimiters
       helm
       helm-projectile
       projectile
       column-marker
	
       ;; Python
       elpy
       flycheck
       py-autopep8

       ;; Haskell
       intero	      ;; IDE
       hlint-refactor ;; Refactoring
       hindent	      ;; Style
       shm            ;; Formatting

       ;; Idris
       idris-mode
       
       ;; Purescript
       purescript-mode
       psc-ide

       ;; coq
       company-coq

       ;; Lisp
       elmacro
       lispy
       evil-lispy

       ;; Rust
       rust-mode
       racer
       flycheck-rust
       cargo

       ;; Markdown
       markdown-mode
       ))

					; list the repositories containing them
(require 'package)
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
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
(setq launch_scripts '(
  "utilities.el"
  "editor.el"
  "haskell.el"
  "markdown.el"
  "python.el"
  "idris.el"
  "lisp.el"
  "coq.el"
  "purescript.el"
  "rust.el"
  "templates.el"
  "org_mode.el"
))

(setq load_script (lambda (file_name)
  (load-file (concat "~/emacs_launch_scripts/" file_name))))

(mapcar load_script launch_scripts)
