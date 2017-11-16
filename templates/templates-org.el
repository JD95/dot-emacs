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
     "#+STARTUP: hidestar"
     "#+STARTUP: indent"
     "\n"
     "# latex options"
     "#+OPTIONS: toc:nil"
     "#+LATEX_HEADER: \\usepackage[margin=1.5in]{geometry}"
     "#+LATEX_HEADER: \\usepackage{apacite}"
     "#+LATEX_HEADER: \\usepackage{setspace}"
    )))
(provide 'templates-org)

;;; templates-org.el ends here
