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

(defun haskell-setup ()
	    (rainbow-delimiters-mode)
	    (drag-stuff-mode)
	    (hlint-refactor-mode)
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

(defun haskell-new-spock-action-component ()
  "Generates a new Haskell Spock api action from template"
  (interactive)
  (let* ((name (read-string "component name: "))
	 (template `("{-# LANGUAGE TypeApplications #-}\n"
		     "{-# LANGUAGE GADTs #-}\n"
		     "{-# LANGUAGE TypeFamilies #-}\n"
		     "{-# LANGUAGE OverloadedStrings #-}\n"
		     "\n"
		     ,(concat "module Actions." name "(" (downcase name) ") where\n")
		     "\n"
		     "import Data.Aeson\n"
		     "import Protolude\n"
		     "import Prelude ()\n"
		     "import Data.HVect\n"
		     "import Web.Spock\n"
		     "\n"
		     "import ServerTypes\n"
		     "import DatabaseTypes\n"
		     "import DefaultResponses\n"
		     "import Utilities\n"
		     "import Database.Persist\n"
		     "import Types\n"
		     "\n"
		     ,(concat (downcase name) " :: ListContains n User xs => API (HVect xs)\n")
		     ,(concat (downcase name) " = pure ()\n")
   		     )))
    (mapc 'insert template))
  )

(provide 'haskell)

;;; haskell.el ends here
