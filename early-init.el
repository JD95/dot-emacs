;; Ensure no GC in startup
(setq gc-cons-threshold most-positive-fixnum)

(defun config-require (symbol)
  (if (boundp symbol)
      (if (eval symbol) "yes" "no")
      (progn
        (with-temp-buffer
          (insert-file-contents (concat user-emacs-directory "settings.el"))
          (insert (concat "(setq " (symbol-name symbol) " nil)\n"))
          (write-region (point-min) (point-max) (concat user-emacs-directory "settings.el")))
        "no")))

(load (concat user-emacs-directory "settings.el"))

(require 'cl-lib)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))

(setq org--inhibit-version-check nil)

(use-package org
  :ensure t
  :config
  (org-babel-do-load-languages
  'org-babel-load-languages
        '((dot . t)
           (emacs-lisp . t)
           (shell . t)))
  (setq org-startup-indented t)
  (org-reload))
