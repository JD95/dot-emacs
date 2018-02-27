;;; templates.el --- Allows for the definitions of file templates

;;; Commentary:
;; 

;;; Code:

(defun template-get-value (pair)
  (read-string (nth 1 pair)))

(defun template-get-values (pairs)
  (mapcar #'template-get-value pairs)) 

(defmacro template-make-lambda (symbols exp)
  `(lambda ,(mapcar #'car symbols) ,exp))

(defun insert-with-newline (value)
  (interactive)
  (insert value)
  (insert "\n"))

(defmacro template-new-template (symbols exp)
  `(let* ((values (template-get-values (quote ,symbols)))
         (template (apply (template-make-lambda ,symbols ,exp) values)))
     (mapc #'insert-with-newline template)))

;; External Config Scripts
(setq templates
  '( "templates-haskell.el"
     "templates-org.el"
     "templates-python.el"
   ))

(setq load_templates (lambda (file_name)
  (load-file (concat "~/emacs_launch_scripts/templates/" file_name))))

(mapcar load_templates templates)

(provide 'templates)

;;; templates.el ends here
