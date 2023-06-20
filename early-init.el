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
