;;; templates-org.el --- A collection of templates for org files

;;; Commentary:
;; 

;;; Code:

(defun template-new-org-latex ()
  (interactive)
  (template-new-template
   ((title "title:") (author "author:") (date "date:"))
   `(,(concat "#+TITLE: " title)
     ,(concat "#+AUTHOR: " author)
     ,(concat "#+DATE: " date)
     "#+OPTIONS: toc:nil"
     "# latex options"
     "#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}"
    )))
(provide 'templates-org)

;;; templates-org.el ends here
