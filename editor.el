(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(smooth-scrolling-mode)
;; Allows for shift+arrow to move between frames
(windmove-default-keybindings)

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
