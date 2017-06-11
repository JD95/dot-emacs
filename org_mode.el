;; Org Mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(add-hook 'org-mode-hook 'visual-line-mode)

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
        ("HOLD" . (:foreground "yellow"))
	("FAILED" . (:foreground "grey"))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "HOLD" "|" "DONE" "FAILED")))

(defun org-todo-sequence ()
  "Creates a list of todo items ending with numbers from start to end"
  (interactive)
  (let* ((item (read-string "todo item: "))
	 (start (string-to-number (read-string "start index: ")))
	 (end (string-to-number (read-string "end index: ")))
	 (todo-item (concat "* TODO " item))
	 (line-item (lambda (i) (concat todo-item " " (number-to-string i) "\n")))
	 (indices (number-sequence start end))
	 (items (mapcar line-item indices)))
    (mapc 'insert items)))

;; Exporting Org files as HTML
(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file))
  (let* ((proj (org-publish-get-project-from-filename buffer-file-name))
         (proj-plist (cdr proj))
         (rel (file-relative-name buffer-file-name
                                  (plist-get proj-plist :base-directory)))
         (dest (plist-get proj-plist :publishing-directory)))
    (browse-url (concat "file://"
                        (file-name-as-directory (expand-file-name dest))
                        (file-name-sans-extension rel)
                        ".html"))))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-;") 'org-twbs-export-to-html)))

;; Org projects
(setq org-publish-project-alist
      '(("programming-tutorials"
         :base-directory "d:/Current_Project/programming-tutorials/Haskell/Chatpers/Absolute Beginners/"
         :publishing-directory "c:/Users/jeffr/Desktop/"
         :publishing-function org-twbs-publish-to-html
         :with-sub-superscript nil
         )))

(eval-after-load "org" '(require 'ox-md nil t))

;; Unicode Symbols
(add-hook 'org-mode-hook
          (lambda ()
            (xah-math-input-mode-on)))
