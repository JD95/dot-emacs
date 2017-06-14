;;; haskell.el --- Utilities for Haskell

;;; Commentary:
;; 

;;; Code:

;; Link to stack binaries
(setq windows-location "~/AppData/Roaming/local/bin")
(setq linux-location "")
(setq macos-location "~/.local/bin")
(setq stack-location (cond ((eq system-type 'windows-nt) windows-location)
			   ((eq system-type 'linux) linux-location)
			   (t macos-location)))

(let ((my-stack-path (expand-file-name stack-location)))
  (setenv "PATH" (concat my-stack-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-stack-path))

;; Haskell Style
(defun haskell-style ()
  "Set the current buffer to use Haskell Style."
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))

;; rainbow delimiter
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (intero-mode)
	    (haskell-style)
	    (rainbow-delimiters-mode)
	    (drag-stuff-mode)
	    (paredit-mode)
	    ))


(eval-after-load 'haskell-mode
  '(progn  (define-key haskell-mode-map (kbd "M-<up>") 'drag-stuff-up)
	   (define-key haskell-mode-map (kbd "M-<down>") 'drag-stuff-down)
	   (define-key haskell-mode-map (kbd "M-<left>") 'drag-stuff-left)
	   (define-key haskell-mode-map (kbd "M-<right>") 'drag-stuff-right)))

(provide 'haskell)

;;; haskell.el ends here
