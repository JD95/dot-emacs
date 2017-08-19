;;; c.el --- Settings for development in C/C++

;;; Commentary:
;; 

;;; Code:

(require 'cl-lib)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(require 'company)
(require 'cc-mode)
(require 'semantic)

(defvar clang-includes
  (system-switch
   '(("D:/msys64/mingw64/include/c++/7.1.0" "D:/msys64/mingw64/include/c++/7.1.0/x86_64-w64-mingw32")
     ("/" "")
     ("" "")
     )))

(defun cpp-config ()
  (interactive)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1)
  (flycheck-mode 1)
  (auto-complete-mode 1)
  (irony-mode 1)
  (company-mode 1)
  (helm-gtags-mode 1)
  (cmake-ide-setup)
;;  (setq flycheck-clang-include-path 'clang-includes)
  )

(add-hook 'c++-mode-hook 'cpp-config)


;;(add-to-list 'company-backends 'company-c-headers)
;;(add-to-list 'company-c-headers-path-system "D:/Program Files/LLVM/lib/clang/4.0.1/include")

(provide 'c)

;;; c.el ends here
