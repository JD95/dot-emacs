;;; templates-haskell.el --- A collection of templates for haskell

;;; Commentary:
;; 

;;; Code:

(defun template-new-haskell-module ()
  (interactive)
  (template-new-template
   ((module "module:") (f "function:"))
   `(,(concat "module " module " where\n")
     "\n"
     ,(concat f " = undefined\n") 
    )))

(provide 'templates-haskell)

;;; templates-haskell.el ends here
