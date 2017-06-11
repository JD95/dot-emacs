(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(smooth-scrolling-mode)

;; Themes and Colors ------------------------------------
(load-theme 'flatland t)

;; WINDOW CONTROLS --------------------------------------

;; Allows for shift+arrow to move between frames
(global-set-key (kbd "C-M-<left>")  'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>")    'windmove-up)
(global-set-key (kbd "C-M-<down>")  'windmove-down)

;; Shrink or grow windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Move using avy search
(global-set-key (kbd "M-s") 'avy-goto-word-1)

;; Swap horizontal and vertical splits
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; For creating new projects

(setq current-projects-directory "E:/Current_Project/")

(setq default-org-header "#+STARTUP: hidestars\n")

(defun new-project ()
  (interactive)
  (let* ((project-name (read-string "project name:"))
	(project-folder (concat current-projects-directory project-name))
	(project-overview (concat project-folder "/overview.org"))
	)
    (make-directory project-folder)
    (append-to-file default-org-header nil project-overview) 
    (find-file project-overview)
  )
)

;; File encodings
(defun unix-file ()
    "Change the current buffer to Latin 1 with Unix line-ends."
    (interactive)
    (set-buffer-file-coding-system 'iso-latin-1-unix t))
(defun dos-file ()
      "Change the current buffer to Latin 1 with DOS line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-dos t))
(defun mac-file ()
      "Change the current buffer to Latin 1 with Mac line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-mac t))

;; Utilities

 (defun insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))
;; File Encryption
(require 'epa-file)
    (epa-file-enable)


(defun epg--list-keys-1 (context name mode)
  (let ((args (append (if (epg-context-home-directory context)
			  (list "--homedir"
				(epg-context-home-directory context)))
		      '("--with-colons" "--no-greeting" "--batch"
			"--with-fingerprint" "--with-fingerprint")
		      (unless (eq (epg-context-protocol context) 'CMS)
			'("--fixed-list-mode"))))
	(list-keys-option (if (memq mode '(t secret))
			      "--list-secret-keys"
			    (if (memq mode '(nil public))
				"--list-keys"
			      "--list-sigs")))
	(coding-system-for-read 'binary)
	keys string field index)
    (if name
	(progn
	  (unless (listp name)
	    (setq name (list name)))
	  (while name
	    (setq args (append args (list list-keys-option (car name)))
		  name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
	     (epg-context-program context)
	     nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
	(setq keys (cons (make-vector 15 nil) keys)
	      string (match-string 0)
	      index 0
	      field 0)
	(while (and (< field (length (car keys)))
		    (eq index
			(string-match "\\([^:]+\\)?:" string index)))
	  (setq index (match-end 0))
	  (aset (car keys) field (match-string 1 string))
	  (setq field (1+ field))))
      (nreverse keys))))

;; Buffer Menu items
(global-set-key "\C-x\C-b" 'buffer-menu)
