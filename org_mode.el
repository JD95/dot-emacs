;; Org Mode

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;; Setting up spell checking
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
 )
)

;; Archives all done todo's in file
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(setq org-todo-keyword-faces
      '(
	("TODO" . (:foreground "red"))
	("PROGRESSING" . (:foreground "orange"))
        ("HOLD" . (:foreground "yellow"))
	("FAILED" . (:foreground "grey"))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "PROGRESSING" "HOLD" "|" "DONE" "FAILED")))

(setq org-scheduled-delay-days 0)

(defun org-todo-sequence ()
  "Creates a list of todo items ending with numbers from start to end"
  (interactive)
  (let* ((item (read-string "todo item: "))
	 (start (string-to-number (read-string "start index: ")))
	 (end (string-to-number (read-string "end index: ")))
	 (todo-item (concat "* TODO " item))
	 (line-item (lambda (i) (concat todo-item " " (number-to-string i) "\n")))
	 (items (mapcar line-item (number-sequence start end))))
    (mapc 'insert items)))

;; Exporting Org files
(defun org-ascii-set-export-options
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-exclude-tags '("no_ascii_export")))
    (funcall orig async subtreep visible-only body-only ext-plist)))

(advice-add 'org-ascii-export-to-ascii
            :around #'org-ascii-set-export-options)

(advice-add 'org-ascii-export-as-ascii
            :around #'org-ascii-set-export-options)


;; html export settings
(defun org-html-set-export-options
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-exclude-tags '("no_html_export")))
    (funcall orig async subtreep visible-only body-only ext-plist)))

(advice-add 'org-html-export-to-html
            :around #'org-html-set-export-options)

(advice-add 'org-html-export-to-html-and-browse
            :around #'org-html-set-export-options)


;; latex export settings
(defun org-latex-set-export-options
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-exclude-tags '("no_latex_export")))
    (funcall orig async subtreep visible-only body-only ext-plist)))

(advice-add 'org-latex-export-to-pdf
            :around #'org-latex-set-export-options)


;; reveal export settings
(defun org-reveal-set-export-options
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-exclude-tags '("no_reveal_export")))
    (funcall orig async subtreep visible-only body-only ext-plist)))

(advice-add 'org-reveal-export-to-html
            :around #'org-reveal-set-export-options)

(advice-add 'org-reveal-export-to-html-and-browse
            :around #'org-reveal-set-export-options)

;; twbs export settings
(defun org-twbs-set-export-options
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-exclude-tags '("no_twbs_export")))
    (funcall orig async subtreep visible-only body-only ext-plist)))

(advice-add 'org-twbs-export-to-html
            :around #'org-twbs-set-export-options)

(advice-add 'org-twbs-export-to-html-and-browse
            :around #'org-twbs-set-export-options)

;; Org projects
(eval-after-load "org" '(require 'ox-md nil t))

(defun org-mode-setup ()
  (local-set-key (kbd "C-;") 'org-twbs-export-to-html)
  (visual-line-mode))

;; Unicode Symbols
(add-hook 'org-mode-hook 'org-mode-setup)

;; org-brain
(setq org-brain-path "F:/brain")
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
(setq org-brain-visualize-default-choices 'all)

(defun aa2u-buffer ()
  (aa2u (point-min) (point-max)))

(add-hook 'org-brain-after-visualize-hook #'aa2u-buffer)

(setq org-priority-faces '(
			   (?0 . (:foreground "green"))
                           (?1 . (:foreground "green yellow" :weight 'bold))
                           (?2 . (:foreground "yellow"))
                           (?3 . (:foreground "gold"))
			   (?4 . (:foreground "orange"))
   			   (?5 . (:foreground "chocolate"))
			   (?6 . (:foreground "firebrick"))
			   (?7 . (:foreground "brown"))
			   (?8 . (:foreground "plum"))
			   (?9 . (:foreground "DarkOrchid4"))
			   ))

;; org-ref
(require 'org-ref)

;; Latex
(setq org-highlight-latex-and-related '(latex script entities))
(setq org-latex-pdf-process
      '("bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""
	"bash -c \"bibtex %b\""
	"bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""
	"bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""
	))
