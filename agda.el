
;; Setup for agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode.exe locate")))
