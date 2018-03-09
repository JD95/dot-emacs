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

(defun template-new-org-comp51-project ()
  (interactive)
  (template-new-template
   ((title "title:"))
   `(,(concat "#+TITLE: " title)
     "#+SUBTITLE: COMP 51"
     "#+STARTUP: hidestar"
     "#+STARTUP: indent"
     ""
     "# latex options"
     "#+OPTIONS: toc:nil num:nil html:postamble:nil date:nil"
     "#+LATEX_HEADER: \\usepackage[margin=1.5in]{geometry}"
     "#+LATEX_HEADER: \\usepackage{setspace}"
     ""
     "* Objective:"
     ""
     "* Description:"
     ""
     "* Requirements:"
     )))

(provide 'templates-org)

(defun template-new-org-reveal-presentation ()
  (interactive)
  (template-new-template
   ((title "title:") (author "author:") (date "date:"))
   `("#+STARTUP: indent"
     "#+STARTUP: hidestar"
     ""
     "#+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.0.0/"
     "#+REVEAL_THEME: serif"
     "#+OPTIONS: num:nil timestamp:nil html-postamble:nil tags:nil toc:nil"
     ""
     ,(concat "#+TITLE: " title)
     ,(concat "#+AUTHOR: " author)
     ,(concat "#+DATE: " date)
     )))


;;; templates-org.el ends here
