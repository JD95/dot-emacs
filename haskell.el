;;; haskell.el --- Utilities for Haskell

;;; Commentary:
;; 

;;; Code:

;; Link to stack binaries
(setq stack-location (gethash "haskell-stack-location" emacs-config))

(let ((my-stack-path (expand-file-name stack-location)))
  (setenv "PATH" (concat my-stack-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-stack-path))

(defun haskell-setup ()
	    (rainbow-delimiters-mode)
	    (drag-stuff-mode)
	    (structured-haskell-mode nil)
	    (intero-mode nil)
	    (global-set-key (kbd "M-s") 'avy-goto-word-1)
	    )

;; rainbow delimiter
(add-hook 'haskell-mode-hook 'haskell-setup)

(require 'shm)
(eval-after-load 'haskell-mode
  '(progn  (define-key haskell-mode-map (kbd "M-<up>") 'drag-stuff-up)
	   (define-key haskell-mode-map (kbd "M-<down>") 'drag-stuff-down)
	   (define-key shm-map (kbd "C-c C-s") 'shm/case-split)	   
	   ))

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))


(provide 'haskell)

;;; haskell.el ends here
