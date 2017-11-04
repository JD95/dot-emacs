(load (gethash "coq-proof-site-location" emacs-config))


(defun coq-setup ()
  (interactive)
  (company-coq-mode)
  (setq buffer-face-mode-face '(:family "FiraCode" :height 120 :width normal))
  (buffer-face-mode))

(add-hook 'coq-mode-hook #'coq-setup)
