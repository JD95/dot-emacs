;; Org Mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key "\C-c\C-l" 'org-twbs-export-to-html)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(add-hook 'org-mode-hook (drag-stuff-mode))

;; predictive install location
(add-to-list 'load-path "~/.emacs.d/predictive/")
;; dictionary locations
(add-to-list 'load-path "~/.emacs.d/predictive/latex/")
(add-to-list 'load-path "~/.emacs.d/predictive/texinfo/")
(add-to-list 'load-path "~/.emacs.d/predictive/html/")
;; load predictive package
(autoload 'predictive-mode "~/.emacs.d/predictive/predictive"
          "Turn on Predictive Completion Mode." t)

;; Fly Spell Mode
;;(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
;;(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
