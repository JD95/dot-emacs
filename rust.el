;;; rust.el --- Configuration for Rust development

;;; Commentary:
;; 



;;; Code:

(require 'rust-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)


(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(define-key rust-mode-map (kbd "C-?") #'racer-describe)
(setq company-tooltip-align-annotations t)

(provide 'rust)

;;; rust.el ends here
