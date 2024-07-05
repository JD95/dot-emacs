;;; -*- lexical-binding: t; byte-compile-warnings: (not docstrings); -*-

(defmacro module (name cond &rest body)
  `(if (and ,@cond)
     (progn
       (message (format "running module %s" ',name))
       ,@body)))

(setq find-location "")

(setq browser-path "c:/Users/jeffr/AppData/Local/Vivaldi/Application/vivaldi.exe")

(setq org-agenda-files
  '("e:/nextcloud/logs.org"
     "e:/nextcloud/projects.org"
     "e:/nextcloud/individual-tasks.org"
     "e:/nextcloud/repeating.org"
     "e:/nextcloud/social.org"
     "e:/nextcloud/reminders.org"
     "e:/nextcloud/refinement.org"
     "e:/nextcloud/events.org"
     "e:/nextcloud/research.org"
     ))

(setq org-id-link-to-org-use-id t)

(setq org-refile-targets
  '((org-agenda-files :maxlevel . 1)))

(setq org-zettelkasten-dir "e:/nextcloud/Zettlekasten")
(setq zettelkasten-journal-path "e:/nextcloud/Zettlekasten/conversations/journal.org")
(setq zettelkasten-people-path "e:/nextcloud/Zettlekasten/17062020123421.org")
(setq z3-solver-cmd "z3")

(setq use-agda t)
(setq use-z3 nil)
(setq use-rust t)
(setq use-nix t)
(setq use-javascript nil)
(setq use-php nil)
(setq use-python nil)
(setq use-purescript nil)
(setq use-idris nil)
(setq use-haskell t)
(setq use-magit t)
(setq use-flyspell nil)


;; Customizing some elisp settings for the rest of
;; the setup.
(module elisp nil

  (setq lisp-indent-offset 2)

  (defun my/elisp-comment-indent-function ()
    "Custom comment indentation function for Emacs Lisp that ignores `comment-column`."
    (if (looking-at "^[ \t]*;;")
      ;; If the comment is at the beginning of a line, keep its indentation.
      (current-indentation)
      ;; Otherwise, align the comment with the code.
      (let ((indent (calculate-lisp-indent)))
        (if (listp indent) (setq indent (car indent)))
        (if (null indent)
          (setq indent (current-indentation)))
        (skip-chars-forward " \t")
        (if (looking-at ";;")
          indent
          (max (1+ (current-column)) 1)))))

  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (set (make-local-variable 'comment-indent-function)
        'my/elisp-comment-indent-function)))

  ;; minor mode for editing parentheses
  (use-package paredit :ensure t))

;; All of the modules for loading emacs packages
;; and startup
(module loading nil

  ;; automatically compile Emacs Lisp libraries
  (use-package auto-compile
    :ensure t
    :config
    (progn
      (auto-compile-on-load-mode)
      (auto-compile-on-save-mode)))

  ;; Some packages need to be installed manually
  ;; so we add a custom folder to the load path
  (add-to-list 'load-path "~/.emacs.d/lisp/")

  (defvar emacs-autosave-directory
    (concat user-emacs-directory "autosaves/")
    "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

  ;; Sets all files to be backed up and auto saved in a single directory.
  (setq backup-directory-alist
    `((".*" . ,emacs-autosave-directory))
    auto-save-file-name-transforms
    `((".*" ,emacs-autosave-directory t)))

  ;; Run garbage collections when frame is not focused
  (add-function :after after-focus-change-function
    (defun my/garbage-collect-maybe ()
      (unless (frame-focus-state)
        (garbage-collect))))

  (dolist (mode
            '(abbrev-mode                  ; E.g. sopl -> System.out.println
               column-number-mode           ; Show column number in mode line
               delete-selection-mode        ; Replace selected text
               dirtrack-mode                ; directory tracking in *shell*
               recentf-mode                 ; Recently opened files
               show-paren-mode              ; Highlight matching parentheses
               ;; which-key-mode ; Available keybindings in popup
	       ))             
    (funcall mode 1))

  (prefer-coding-system 'utf-8)

  (when browser-path
    (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program browser-path)))

(use-package exec-path-from-shell :ensure t)

(setq
  ;; Refresh buffers fast
  auto-revert-interval 1
  ;; Discard customization's
  custom-file (make-temp-file "")
  ;; Use TeX when toggling input method
  default-input-method "TeX"
  ;; Show keystrokes asap
  echo-keystrokes 0.1
  ;; No splash screen please
  inhibit-startup-message t
  ;; Clean scratch buffer
  initial-scratch-message nil
  ;; Show more recent files
  recentf-max-saved-items 100
  ;; Quiet
  ring-bell-function 'ignore
  ;; No double space
  sentence-end-double-space nil)

(setq-default
  ;; Use spaces instead of tabs
  indent-tabs-mode nil
  ;; Split verticly by default
  split-width-threshold 160
  ;; Split verticly by default
  split-height-threshold nil)

;; Disable narrow-to-region builds
(put 'narrow-to-region 'disabled nil)

(module os-specific-config nil

  (module macos-config ((memq window-system '(mac ns)))
    (defvar select-enable-clipboard t)
    (defvar mac-option-key-is-meta nil)
    (defvar mac-command-key-is-meta t)
    (defvar mac-command-modifier 'meta)
    (defvar mac-option-modifier nil)

    (exec-path-from-shell-initialize)
    (grep-compute-defaults)

    (when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode 1))

    (when (boundp 'mac-pass-command-to-system)
      (setq mac-pass-command-to-system nil)))

  (module linux-config ((eq window-system 'gnu/linux))
    (exec-path-from-shell-initialize))

  (module windows-config ((eq window-system 'w32))
    (grep-compute-defaults)))

;; Modular text completion framework
(use-package company
  :ensure t
  :config
  (global-company-mode 1))

;; Emacs support library for PDF files
(use-package pdf-tools :ensure t)

(module appearance nil

  (dolist (mode
            '(tool-bar-mode                ; No toolbars, more room for text
               scroll-bar-mode             ; No scroll bars either
               menu-bar-mode               ; No menubar
               blink-cursor-mode))         ; The blinking cursor gets old
    (funcall mode 0))

  (use-package color-theme-sanityinc-tomorrow :ensure t)
  (use-package solarized-theme :ensure t)

  (setq custom-safe-themes
    '("f36a31fc61d6eba25b68df592577d80cfe471b7673575e3af8005cd32416ab33" default))

  (require 'color-theme-sanityinc-tomorrow)

  (if (display-graphic-p)
    (color-theme-sanityinc-tomorrow-night)
    (load-theme 'solarized t))

  (cond ((member "DejaVu Sans Mono" (font-family-list))
          (set-face-attribute 'default nil :font "DejaVu Sans Mono")
          (set-frame-font "DejaVu Sans Mono" nil t)))

  (use-package unicode-fonts :ensure t)
  (unicode-fonts-setup)

  ;; Use this font if the current font can't render a symbol
  (set-fontset-font "fontset-default" 'unicode "DejaVu Sans")


  ;; Diminished modes from modeline
  (use-package diminish
    :ensure t
    :config
    (progn

      (defmacro safe-diminish (file mode &optional new-name)
        `(with-eval-after-load ,file
           (diminish ,mode ,new-name)))

      (safe-diminish "eldoc" 'eldoc-mode)
      (safe-diminish "flyspell" 'flyspell-mode)
      (safe-diminish "projectile" 'projectile-mode)
      (safe-diminish "paredit" 'paredit-mode "()")))

  ;; Increase selected region by semantic units
  (use-package expand-region :ensure t)

  (use-package git-gutter-fringe
    :ensure t
    :config
    (global-git-gutter-mode 1)
    (require 'git-gutter-fringe)

    (dolist (p '((git-gutter:added    . "#0c0")
                  (git-gutter:deleted  . "#c00")
                  (git-gutter:modified . "#c0c")))
      (set-face-foreground (car p) (cdr p))
      (set-face-background (car p) (cdr p)))))

(module control nil

  

  (module motion nil
    (use-package avy
      :ensure t
      :config
      (progn
        ))

    (use-package smooth-scrolling
      :ensure t
      :config
      (smooth-scrolling-mode 1))

    (use-package undo-tree
      :ensure t)

    (global-undo-tree-mode)

    (use-package evil
      :ensure t
      :init
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1)
      (evil-set-undo-system 'undo-tree))

    (use-package evil-collection
      :after evil
      :ensure t
      :config
      (evil-collection-init))

    (defun my/kill-this-buffer-unless-scratch ()
      "Works like `kill-this-buffer' unless the current buffer is the
,*scratch* buffer. In witch case the buffer content is deleted and
the buffer is buried."
      (interactive)
      (if (not (string= (buffer-name) "*scratch*"))
        (kill-this-buffer)
        (delete-region (point-min) (point-max))
        (switch-to-buffer (other-buffer))
        (bury-buffer "*scratch*")))

    (defun my/find-first-non-ascii-char ()
      "Find the first non-ascii character from point onwards."
      (interactive)
      (let (point)
        (save-excursion
          (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                      'ascii)
                  (throw 'non-ascii (point)))
                (forward-char 1)))))
        (if point
          (goto-char point)
          (message "No non-ascii characters.")))))

  (module editing nil

    (module sorting nil

      (defun my/sort-words-in-region (start end)
        "Sort the words in a given region (START and END) and return them as a list."
        (sort (split-string (buffer-substring-no-properties start end)) #'string<))

      (defun my/sort-words-sorted (start end)
        "Sort the words in a given region (START and END) and return them as a string."
        (mapconcat 'identity (my/sort-words-in-region start end) " "))

      (defun my/sort-words (start end)
        "Sort words in region alphabetically. Then insert them replacing the existing region. START and END are boundries of the selected region."
        (interactive "r")
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (let ((words (my/sort-words-sorted (point-min) (point-max))))
              (delete-region (point-min) (point-max))
              (goto-char (point-min))
              (insert words))))))

    (module inserts nil

      (defun my/insert-current-date (&optional omit-day-of-week-p)
        "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
        (interactive "P*")
        (insert (calendar-date-string (calendar-current-date) nil
                  omit-day-of-week-p)))


      (defvar my/current-time-format "%H:%M:%S"
        "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

      (defun my/insert-current-time ()
        "Insert the current time."
        (interactive)
        (insert (format-time-string my/current-time-format (current-time))))

      (defun my/current-date ()
        (calendar-date-string
          (calendar-current-date)
          nil))

      (defun my/start-of-week ()
        (calendar-date-string
          (calendar-gregorian-from-absolute
            (calendar-dayname-on-or-before
              0 ; Sunday
              (calendar-absolute-from-gregorian (calendar-current-date))))))

      (defun my/current-month ()
        (let ((dt (calendar-current-date)))
          (format "%s %s" (calendar-month-name (nth 0 dt)) (nth 2 dt))))))

  (defun my/toggle-window-split ()
    "Change the window split from horizontal to vertical."
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

  )

(module minor-modes nil

  (module projectile nil
    
    (use-package projectile
      :ensure t)
    (use-package counsel-projectile
      :ensure t)

    (setq find-program find-location)

    (require 'projectile)
    (projectile-mode 1)
    (counsel-projectile-mode))

  (module ivy nil

    (use-package ivy
      :ensure t
      :config (setq ivy-use-virtual-buffers t
                ivy-use-selectable-prompt t
                enable-recursive-minibuffers t
                ivy-display-style 'fancy
                ivy-count-format "(%d/%d)"
                ivy-wrap t))

    (use-package ivy-hydra
      :ensure t)

    (use-package counsel :ensure t)

    (setq ivy-re-builders-alist
      '((swiper-isearch . ivy--regex)
         (counsel-projectile-find-file . ivy--regex)
         (t             . ivy--regex-fuzzy))))

  (module prescient nil
    (use-package prescient
      :ensure t)
    (use-package ivy-prescient
      :ensure t
      :after counsel
      :config
      (ivy-prescient-mode 1)
      (prescient-persist-mode 1))

    (setq prescient-history-length 100)
    (setq prescient-frequency-decay 0.997)
    (setq prescient-frequency-threshold 0.5)
    (setq prescient-sort-length-enable t)

    ;; There are several methods that can be used to filter prescient results:
    ;; - literal: query is a literal sub-strings of the candidate
    ;; - initalism: query is initials of the candidate
    ;; - prefix: query is beginnings of parts of a candidate
    ;; - anchored: parts of query are separated by capital letters, similar to prefix with no spaces
    ;; - fuzzy: character of query exist in candidate in right order
    ;; - regexp: the query is a regexp, find all matches
    (setq ivy-prescient-enable-filtering t)
    (setq prescient-filter-method '(literal regexp fuzzy))
    (setq ivy-prescient-sort-commands '(:not ivy-switch-buffer swiper-isearch)))

  (module hydra nil

    (use-package hydra :ensure t)

    (require 'hydra)

    (defhydra hydra-zoom nil
      "zoom"
      ("k" text-scale-increase "in")
      ("j" text-scale-decrease "out"))


    (defhydra hydra-window ()
      "
      Movement^^        ^Split^               ^Resize^
      ---------------------------------------------------
      _h_ ←           _v_ertical      _H_ X←
      _j_ ↓           _x_ horizontal  _J_ X↓
      _k_ ↑           _S_ave          _K_ X↑
      _l_ →           _o_nly this     _L_ X→
      _d_lt Other
      _SPC_ cancel
      "
      ("h" windmove-left )
      ("j" windmove-down )
      ("k" windmove-up )
      ("l" windmove-right )
      ("H" evil-window-decrease-width)
      ("J" evil-window-decrease-height)
      ("K" evil-window-increase-height)
      ("L" evil-window-increase-width)
      ("v" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right))
        )
      ("x" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down))
        )
      ("S" save-buffer)
      ("d" delete-window)
      ("o" delete-other-windows)
      ("SPC" nil))

    ))


(module org nil

  (use-package ox-twbs
    :ensure t)
  (use-package evil-org
    :ensure t)
  (use-package org-ql
    :ensure t)
  (use-package org-recur
    :hook ((org-mode . org-recur-mode)
            (org-agenda-mode . org-recur-agenda-mode))
    :demand t
    :config
    (setq org-recur-finish-done t
      org-recur-finish-archive t))

  (setq org-modules '(org-habit))


  (defun compress-org-link (arg)
    (interactive "P")
    (let ((url (thing-at-point 'url))
           (bounds (bounds-of-thing-at-point 'url)))
      (kill-region (car bounds) (cdr bounds))
      (insert (format "[[%s][%s]]" url
                (truncate-string-to-width url
                  (if arg
                    (prefix-numeric-value arg)
                    40)
                  nil nil "...")))))

  (setq org-structure-template-alist
    '(("a" . "export ascii")
       ("c" . "center")
       ("C" . "comment")
       ("e" . "example")
       ("E" . "export")
       ("h" . "export html")
       ("l" . "export latex")
       ("q" . "quote")
       ("s" . "src")
       ("v" . "verse")))

  (setq org-fold-catch-invisible-edits 'error)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)

  (defun org-convert-markdown-links-to-org ()
    (interactive)
    (query-replace-regexp "\\[\\(.+\\)\\](\\(.*\\))" "[[\\2][\\1]]" nil (region-beginning) (region-end)))



  (defun org-replace-file-path-with-todo-link (start end)
    (interactive "r")
    (query-replace-regexp
      (rx (0+ any)
        (or (: (= 2 (in digit)) ":" (= 2 (in digit)))
          (: (= 4 (in digit)) "-" (= 2 (in digit)) "-" (= 2 (in digit))))
        (1+ " ")
        (: (group (0+ any)) "." (group (or "pdf" "epub" "djvu"))))
      "** READY [[file:./\\1.\\2][\\1]]"
      nil start end))


  (require 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional))


  (with-eval-after-load 'org
    (advice-add 'org-set-tags-command :override #'counsel-org-tag))

  (defun org-apply-tags-to-children ()
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (let* ((depth (+ 1 (org-current-level))))
        (org-map-entries
          (lambda ()
            (when (<= (org-current-level) depth)
              (org-set-tags (org-get-tags))))
          nil
          'tree))))


  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


  (module org-agenda nil

    (setq org-agenda-custom-commands
      '(("!" "Expired Deadlines" tags-todo
          "+DEADLINE<\"<today>\"/TODO|PROGRESSING"
          nil nil)
         ("p" "In Progress" tags-todo
           "/PROGRESSING|RESEARCHING|SHOPPING")
         ("d" "Scheduled or Deadline" agenda
           "+DEADLINE+SCHEDULED"
           nil nil)
         ("u" "Upcoming Events"
           ((org-ql-block '(and (ts :from today :to +62) (tags "Event")))))))


    (add-hook 'org-agenda-finalize-hook #'hl-line-mode)

    (setq org-scheduled-delay-days 0)

    (setq org-agenda-hide-tags-regexp ".*")

    (require 'org-habit)
    (set-face-attribute 'org-habit-overdue-face nil :foreground "gray5" :background "#D84747")
    (set-face-attribute 'org-habit-overdue-future-face nil :foreground "gray5" :background "#D84747")
    (set-face-attribute 'org-habit-alert-face nil :foreground "gray5" :background "#FFE438")
    (set-face-attribute 'org-habit-alert-future-face nil :foreground "gray5" :background "#FFE438")
    (set-face-attribute 'org-habit-ready-face nil :foreground "gray5" :background "#53C65F")
    (set-face-attribute 'org-habit-ready-future-face nil :foreground "gray5" :background "#53C65F")
    (set-face-attribute 'org-habit-clear-face nil :foreground "gray5" :background "#20A4F3")
    (set-face-attribute 'org-habit-clear-future-face nil :foreground "gray5" :background "#20A4F3")
    (setq org-habit-today-glyph (make-glyph-code ?!))
    (setq org-habit-completed-glyph (make-glyph-code ?*))
    (setq org-habit-graph-column 45)
    (setq org-habit-preceding-days 14)
    (setq org-habit-following-days 3)


    (use-package org-edna
      :ensure t)

    (require 'org-edna)
    (org-edna-mode)

    (setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red"))
         ("DEVELOPING" . (:foreground "DarkRed"))
         ("PROGRESSING" . (:foreground "orange"))
         ("RESEARCHING" . (:foreground "orange"))
         ("UNVERIFIED" . (:foreground "indian red"))
         ("HOLD" . (:foreground "yellow"))
         ("FAILED" . (:foreground "grey"))
         ("BUY" . (:foreground "pink"))
         ("SHOPPING" . (:foreground "orange"))
         ("ORDERED" . (:foreground "sky blue"))
         ("BOUGHT" . (:foreground "green"))
         ("CANCELED" . (:foreground "grey"))
         ("VERIFIED" . (:foreground "green"))
         ("ANSWERED" . (:foreground "green"))))

    (setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
         (sequence "DEVELOPING(v)" "READY(y)" "PROGRESSING(p)" "HOLD(h)" "|" "FINISHED(f)")
         (sequence "UNKNOWN(u)" "RESEARCHING(r)" "|" "ANSWERED(a)")
         (sequence "BUY(b)" "SHOPPING(s)" "ORDERED(o)" "|" "BOUGHT(h)")
         (sequence "|" "FAILED(a)" "CANCELED(c)")))


    (setq org-priority-faces
      '((?A . (:foreground "#fc0303"))
         (?B . (:foreground "#db792e"))
         (?C . (:foreground "#ebe06a"))
         (?D . (:foreground "#56fc6c"))
         (?E . (:foreground "#00fbff"))
         (?F . (:foreground "#ad61ff"))))

    (setq org-priority-highest ?A)
    (setq org-priority-lowest ?Z)
    (setq org-priority-default ?Z)
    (setq org-log-into-drawer 'LOGBOOK)

    (setq org-log-into-drawer 'LOGBOOK)
    (setq org-todo-heirarchical-statistics nil)
    (setq org-log-done 'time)

    (defun detect-org-recur-advice (orig &rest all)
      (interactive)
      (let ((header-text (nth 4 (org-heading-components))))
        (if (seq-contains-p header-text ?|)
          (progn
            (org-recur-finish)
            (read-char "Task completed! Press any key to continue...") nil 1)
          (apply orig all))))

    (advice-add
      #'org-todo :around
      #'detect-org-recur-advice)

    (defun org-call-header-action-advice (orig &rest all)
      (interactive)
      (let ((action-property (org-entry-get (point) "action")))
        (if action-property
          (let ((string-action (format "(progn %s)" action-property)))
         ; Call the action and it's result says if org-todo
         ; should be called
            (when (eval (car (read-from-string string-action)))
              (apply orig all)))
          (apply orig all))))

    (advice-add
      #'org-todo :around
      #'org-call-header-action-advice)

    (defun org-todo-action-iter-property (property func)
      (save-excursion
        (let ((value (org-entry-get (point) property)))
          (org-entry-put (point) property (funcall func value)))))

    (defun org-todo-inc-mod (property mod-base)
      (org-todo-action-iter-property property
        (lambda (x)
          (number-to-string (mod (+ (string-to-number x) 1) mod-base)))))

    (defun org-todo-action-shift-next-date (num-days)
      (save-excursion
        (org-todo-action-iter-property
          "next-date"
          (lambda (date)
            (with-temp-buffer
              (insert date)
              (goto-char 10) ;; This would be the day in a timestamp
              (org-timestamp-up num-days) ;; Shift this many days
              (buffer-string))))))

    (defun org-todo-plan-for (thing num-days)
      (progn
        (when (string= "DONE" (nth 2 (org-heading-components)))
          (org-todo-action-shift-next-date num-days)
          (let* ((new-date (save-excursion (org-entry-get (point) "next-date")))
                  (new-heading (format "Plan %s for %s" thing new-date)))
            (org-edit-headline new-heading)))
        'continue))

    (defun org-todo-count-n (n-times)
      (save-excursion
        (if (string= "TODO" (nth 2 (org-heading-components)))
          (progn
            (org-todo-inc-mod "count" n-times)
            (let* ((new-count (save-excursion (org-entry-get (point) "count")))
                    (old-heading (nth 4 (org-heading-components)))
                    (heading-text (string-trim-right (car (split-string old-heading "\\["))))
                    (new-heading (format "%s [%s/%d]" heading-text new-count n-times))
                    (should-continue (string= "0" new-count)))
              (progn
                (org-edit-headline (if should-continue heading-text new-heading))
                should-continue)))
          'continue)))

    (defun org-map-tasks (action state)
      (org-map-entries
        (lambda ()
          (funcall action)
          (setq org-map-continue-from (outline-previous-heading)))
        state 'file))

    (defun org-archive-done-tasks ()
      (interactive)
      (org-map-tasks 'org-archive-subtree "/DONE"))

    (defun org-cut-done-tasks ()
      (interactive)
      (org-map-tasks 'org-cut-subtree "/DONE"))


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

    
    (defun org-sort-alpha-todo ()
      (interactive)
      (org-sort-entries nil ?a)
      (org-sort-entries nil ?p)
      (org-sort-entries nil ?o))

    (setq org-hierarchical-todo-statistics nil)


    (defun org-set-daily-subtask-deadline ()
      (interactive)
      (save-excursion
        (org-back-to-heading t)
        (let* ((depth (+ 1 (org-current-level))))
          (org-map-entries
            (lambda ()
              (when (<= (org-current-level) depth)
                (org-set-property "TRIGGER" "next-sibling deadline!(\"++1d\")")))
            nil
            'tree))))

    (defun org-edna-set-time (type last-entry args)
      (ignore last-entry)
      (let* ((new-time (nth 0 args))
              (prev-ts (org-edna--get-planning-info type))
              (time-val (split-string new-time ":"))
              (ts (org-read-date-analyze prev-ts nil '(nil nil nil nil nil nil))))
        (setcar (nthcdr 2 ts) (string-to-number (nth 0 time-val)))
        (setcar (nthcdr 1 ts) (string-to-number (nth 1 time-val)))
        (org--deadline-or-schedule nil type
          (format-time-string "%F %R" (apply 'encode-time (mapcar (lambda (e) (or e 0)) ts))))))

    (defun org-edna-action/schedule-time! (last-entry &rest args)
      (org-edna-set-time 'scheduled last-entry args))

    (defun org-edna-action/deadline-time! (last-entry &rest args)
      (org-edna-set-time 'deadline last-entry args))

    (defun org-goto-random-subtree (todo-pred &optional depth)
      (interactive)
      (org-back-to-heading t)
      (let ((todos '())
             (top-level (org-current-level)))
        ;; Gather todo headings
        (org-map-tree
          (lambda () (when (and
                             (if depth
                               (<= (org-current-level) (+ top-level depth))
                               t)
                             (funcall todo-pred))
                       (setq todos (cons (point) todos)))))
        ;; Randomly select one
        (goto-char
          (nth (random (- (length todos) 1)) todos)))
      )

    (defun at-todo (exclude-states)
      (and (org-get-todo-state)
        (member (org-get-todo-state) exclude-states)))

    (defun org-goto-random-subtree-todo ()
      (interactive)
      (org-goto-random-subtree
        (lambda () (at-todo '("TODO" "DEVELOPING" "PROGRESSING")))))

    (defun org-goto-random-subtree-todo-below ()
      (interactive)
      (org-goto-random-subtree
        (lambda () (at-todo '("TODO" "DEVELOPING" "PROGRESSING")))
        1))

    (defun force-double-digit-format-advice (orig fmt &rest args)
      (if (string= fmt "[%d%%]")
        (apply #'format "[%02d%%]" args)
        (apply orig fmt args)))

    (defun org-update-parent-todo-statistics-advice (orig &rest all)
      (progn
        (advice-add
          #'format :around
          #'force-double-digit-format-advice)
        (apply orig all)
        (advice-remove
          #'format
          #'force-double-digit-format-advice)))

    (advice-add
      #'org-update-parent-todo-statistics :around
      #'org-update-parent-todo-statistics-advice)

    (use-package org-super-agenda
      :ensure t)

    (defun my-org-agenda-remove-recur (str)
      (nth 0 (split-string str "|")))

    (setq org-super-agenda-groups '(
                                     (:name "Short Term Wellfare"
                                       :priority "A"
                                       :transformer #'my-org-agenda-remove-recur)
                                     (:name "Long Term Wellfare"
                                       :priority "B"
                                       :transformer #'my-org-agenda-remove-recur)
                                     (:name "Relationships"
                                       :priority "C"
                                       :transformer #'my-org-agenda-remove-recur)
                                     (:name "Actualization"
                                       :priority "D"
                                       :transformer #'my-org-agenda-remove-recur)
                                     (:name "Refinement"
                                       :priority "F"
                                       :transformer #'my-org-agenda-remove-recur)
                                     ))
    (org-super-agenda-mode)

    (defun org-force-open-current-window ()
      (interactive)
      (let ((org-link-frame-setup (quote
                                    ((vm . vm-visit-folder)
                                      (vm-imap . vm-visit-imap-folder)
                                      (gnus . gnus)
                                      (file . find-file)
                                      (wl . wl)))
              ))
        (org-open-at-point)))
    ;; Depending on universal argument try opening link
    (defun org-open-maybe (&optional arg)
      (interactive "P")
      (if arg
        (org-open-at-point)
        (org-force-open-current-window)
        )
      )

    (setq org-link-file-path-type 'relative)

    (setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

    (ignore-errors (eval-after-load "org"
                     '(progn
                        (require 'ob-shell)
                        (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,")
                        (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)))))

    (setq org-fontify-quote-and-verse-blocks t)
    (custom-set-faces
      '(org-block-begin-line
         ((t (:foreground "#D6D6D6" :background "#2e373b"))))
      '(org-block
         ((t (:foreground "#70acc2"))))
      '(org-block-end-line
         ((t (:foreground "#D6D6D6" :background "#2e373b"))))
      )

    (defun sudo-shell-command (command)
      (interactive "MShell command (root): ")
      (with-temp-buffer
        (cd "/sudo::/")
        (async-shell-command command)))
    (defun org-tangle-sh ()
      (interactive)
      (find-file buffer-file-name)
      (org-babel-tangle)
      (let ((script-file (concat
                           (car (split-string buffer-file-name ".org")) ".sh")))
        (sudo-shell-command
          (concat "chmod +x " script-file " && " "./" script-file))
        )
      )

    (defun org-ascii-set-export-options
      (orig &optional async subtreep visible-only body-only ext-plist)
      (let ((org-export-exclude-tags '("no_ascii_export")))
        (funcall orig async subtreep visible-only body-only ext-plist)))

    (advice-add 'org-ascii-export-to-ascii
      :around #'org-ascii-set-export-options)

    (advice-add 'org-ascii-export-as-ascii
      :around #'org-ascii-set-export-options)

    (defun org-html-set-export-options
      (orig &optional async subtreep visible-only body-only ext-plist)
      (let ((org-export-exclude-tags '("no_html_export")))
        (funcall orig async subtreep visible-only body-only ext-plist)))

    (advice-add 'org-html-export-to-html
      :around #'org-html-set-export-options)

    (advice-add 'org-html-export-to-html-and-browse
      :around #'org-html-set-export-options)

    (defun org-latex-set-export-options
      (orig &optional async subtreep visible-only body-only ext-plist)
      (let ((org-export-exclude-tags '("no_latex_export")))
        (funcall orig async subtreep visible-only body-only ext-plist)))

    (advice-add 'org-latex-export-to-pdf
      :around #'org-latex-set-export-options)

    (defun org-reveal-set-export-options
      (orig &optional async subtreep visible-only body-only ext-plist)
      (let ((org-export-exclude-tags '("no_reveal_export")))
        (funcall orig async subtreep visible-only body-only ext-plist)))

    (advice-add 'org-reveal-export-to-html
      :around #'org-reveal-set-export-options)

    (advice-add 'org-reveal-export-to-html-and-browse
      :around #'org-reveal-set-export-options)

    (defun org-twbs-set-export-options
      (orig &optional async subtreep visible-only body-only ext-plist)
      (let ((org-export-exclude-tags '("no_twbs_export")))
        (funcall orig async subtreep visible-only body-only ext-plist)))

    (advice-add 'org-twbs-export-to-html
      :around #'org-twbs-set-export-options)

    (advice-add 'org-twbs-export-to-html-and-browse
      :around #'org-twbs-set-export-options)

    (defun render-org-twbs (org-file)
      "Render the given org file as html using twbs."

      (interactive "f\nf")

      (unless (string= "org" (file-name-extension org-file))
        (error "INFILE must be an org file."))

      (let* ;; Check if the file is already open
        ((open (find-buffer-visiting org-file))

          ;; Load the file into a buffer
          (org-file-buffer (find-file-noselect org-file))

          ;; Switch to temp buffers to render contents of org file
          (result (with-current-buffer org-file-buffer
                    ;; Temporarily use the org-file-buffer and render
                    (org-twbs-export-as-html nil nil t t)
                    (with-current-buffer "*Org HTML Export*"
                      ;; Temporarily switch and get result of render
                      (buffer-string)))))

        (kill-buffer "*Org HTML Export*")

        ;; Don't kill the buffer if it was open previously
        (unless open (kill-buffer org-file-buffer))

        result))

    (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

    (setq-default bibtex-dialect 'biblatex)

    (setq org-highlight-latex-and-related '(latex script entities))

    ;; linux commands are run via a wsl distro with nix setup
    ;; the commands are run in a shell with the various packages made available
    (defun my/wsl-command (cmd)
      (concat
        "wsl.exe -d nix-ubuntu -u jeff -e /nix/var/nix/profiles/default/bin/nix-shell --command \""
        cmd
        "\" /home/jeff/shell.nix"))

    (setq org-latex-pdf-process
      `(,(my/wsl-command "lualatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
    (setq org-image-actual-width nil)

    (setq org-preview-latex-default-process 'dvisvgm)

    ;; The files generated by intermediate steps are sent to the tmp folder
    ;; No logs are output to emacs so it's best to just route to a log file in tmp as well
    ;; :image-converter (,(my/wsl-command "dvisvgm %f --no-fonts --exact-bbox --scale='1.0' --output=%o/%b.svg > /mnt/d/Temp/test.log 2>&1"))
    (setq org-preview-latex-process-alist
      `((dvisvgm :description "dvi > svg"
          :message "you need to install the programs: latex and dvisvgm."
          :programs nil
          :post-clear nil
          :image-input-type "dvi"
          :image-output-type "svg"
          :image-size-adjust (3.0 . 1.0)
          :latex-compiler (,(my/wsl-command "latex -interaction nonstopmode -output-directory %o %f"))
          :image-converter (,(my/wsl-command "dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%o/%b.svg")))))

    (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
    (setq org-refile-use-outline-path 'file)


    (defun org-zettelkasten-get-file-id ()
      (let ((path (buffer-file-name)))
        (with-temp-buffer
          (insert path)
          (goto-char (point-min))
          (while (re-search-forward
                   (rx (char alpha) ":/"
                     (1+ (1+ (char word)) "/")
                     (group (1+ digit))
                     ".org")
                   nil t)
            (replace-match "\\1"))
          (buffer-string))))

    (defun org-zettelkasten-pick-tag ()
      (let* ((tag-file-buffer (find-file (counsel-find-file (concat org-zettelkasten-dir "/zettl-tags/"))))
              (name (buffer-file-name tag-file-buffer)))
        (kill-buffer tag-file-buffer)
        name))

    (defun org-zettelkasten-append-tag (tag-file-name buff-name link-text)
      (append-to-file (concat "[[file:../" buff-name "][\n" link-text "]]") nil
        tag-file-name))

    (defun first-line-of-buffer-or-name ()
      (let* ((text (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
              (first-line (car (delete "" text))))
        (if (string= first-line "") (buffer-name) first-line)))

    (defun org-zettelkasten-buffer-name ()
      (interactive)
      (rename-buffer (first-line-of-buffer-or-name)))

    (defun org-zettelkasten-add-tag-if-missing (tag-file-name title)
      (let* ((id (org-zettelkasten-get-file-id)))
        (with-temp-buffer
          (when (file-exists-p tag-file-name)
            (insert-file-contents tag-file-name))
          (when (not (re-search-forward id nil t))
            (org-zettelkasten-append-tag tag-file-name (concat id ".org") title)))))

    (defun org-zettelkasten-add-tag ()
      (interactive)
      (save-window-excursion
        (org-zettelkasten-add-tag-if-missing
          (org-zettelkasten-pick-tag)
          (first-line-of-buffer-or-name))))

    (defun org-zettelkasten-list-tags ()
      (interactive)
      (lgrep (buffer-name) "*.org" (concat org-zettelkasten-dir "/zettl-tags")))

    (defun org-zettelkasten-copy-tags-from-grep ()
      (interactive)
      (mapc (lambda (tag-file-name)
              (org-zettelkasten-append-tag
                (concat org-zettelkasten-dir "/zettl-tags/" tag-file-name)
                (buffer-name)
                (first-line-of-buffer-or-name)))
        (apply #'append (with-current-buffer "*grep*"
                          (s-match-strings-all
                            (rx (and line-start (+ (or letter digit "-")) ".org"))
                            (buffer-substring-no-properties (point-min) (point-max)))))))

    (defun org-zettelkasten-list-external-refs ()
      (interactive)
      (lgrep (file-name-nondirectory (buffer-file-name)) "*.org" (concat org-zettelkasten-dir)))

    (defun org-zettelkasten-search ()
      (interactive)
      (lgrep (read-string "query: ") "*.org" org-zettelkasten-dir))

    (defun org-zettelkasten-new-note ()
      (interactive)
      (find-file (concat org-zettelkasten-dir "/"
                   (format-time-string "%d%m%Y%H%M%S" (current-time)) ".org")))

    (defun org-zettelkasten-gen-id ()
      (interactive)
      (insert (format-time-string "%d%m%Y%H%M%S" (current-time))))

    (defun org-zettelkasten-conversation ()
      (interactive)
      (counsel-find-file (concat org-zettelkasten-dir "/conversations/")))

    (defun org-zettelkasten-conversation-branch ()
      (interactive)
      (let* ((new-note-name (concat (format-time-string "%d%m%Y%H%M%S" (current-time)) ".org"))
              (new-note-path (concat org-zettelkasten-dir "/" new-note-name))
              (new-note-desc (if (use-region-p)
                               (progn
                                 (kill-region (region-beginning) (region-end))
                                 (concat "][" (car kill-ring) "]]"))
                               "]]"))
              (new-note-link (concat "[[file:" (file-relative-name new-note-path default-directory) new-note-desc))
              (prev-note-title (first-line-of-buffer-or-name))
              (prev-note-link (concat "[[file:" (file-relative-name (buffer-file-name) org-zettelkasten-dir) "][" prev-note-title "]]")))
        (insert new-note-link)
        (save-buffer)
        (find-file new-note-path)
        (insert (concat (car kill-ring) "\n\nprevious:\n" prev-note-link))
        (org-zettelkasten-buffer-name)))


    (setq org-zettelkasten-ref nil)

    (defun org-zettelkasten-grab-ref ()
      (interactive)
      (setq org-zettelkasten-ref (buffer-file-name)))

    (defun org-zettelkasten-use-ref ()
      (interactive)
      (let* ((link-end (if (use-region-p)
                         (progn
                           (kill-region (region-beginning) (region-end))
                           (concat "][" (car kill-ring) "]]"))
                         "]]")))
        (insert (concat "[[file:" (file-relative-name org-zettelkasten-ref default-directory) link-end)))
      )

    (defun org-zettelkasten-publish-to-blog ()
      (interactive)
      (let* ((input (buffer-file-name))
              (id-regex (rx (1+ (in digit)) ".org"))
              (id (substring input (string-match id-regex input) -4))
              (old-buffer (buffer-name)))
        (save-window-excursion
          (org-zettelkasten-add-tag-if-missing
            (concat org-zettelkasten-dir "/zettl-tags/blog-published.org")
            (first-line-of-buffer-or-name))
          (with-temp-buffer
            (insert-buffer-substring old-buffer)
            (goto-char (point-min))
            (insert "#+TITLE: ")
            (org-html-export-as-html))
          (switch-to-buffer "*Org HTML Export*")
          (write-region
            (point-min) (point-max)
            (concat "d:/current-projects/blog/public/" id ".html"))
          (with-temp-buffer
            (insert-file-contents
              (concat org-zettelkasten-dir "/zettl-tags/blog-published.org"))
            (while (re-search-forward (rx "../" (group (1+ (in digit))) ".org") nil t)
              (replace-match "\\1.html"))
            (goto-char (point-min))
            (while (re-search-forward (rx "[[") nil t)
              (replace-match "\n- [["))
            (org-html-export-as-html))
          (switch-to-buffer "*Org HTML Export*")
          (write-region
            (point-min) (point-max)
            "d:/current-projects/blog/public/index.html")
          (kill-matching-buffers "\\*Org HTML Export\\*" nil t))))


    (defhydra hydra-zettelkasten (:hint nil)
      "
      ^Notes^         ^Refs^         ^Tags^                    ^Search^
      ----------------------------------------------------------------------
      _n_: New Note   _g_: Grab ref   _t_: Add Tag              _s_: Search
      _c_: New Convo  _u_: Use Ref    _l_: List Tags            _e_: Links to this Note
      _b_: Branch     ^ ^             _C_: Copy Tags from Grep  _p_: Publish to Blog
      _r_: Rename Buffer
      "
      ("n" org-zettelkasten-new-note)
      ("c" org-zettelkasten-conversation)
      ("b" org-zettelkasten-conversation-branch)
      ("r" org-zettelkasten-buffer-name)

      ("t" org-zettelkasten-add-tag)
      ("l" org-zettelkasten-list-tags)
      ("C" org-zettelkasten-copy-tags-from-grep)

      ("s" org-zettelkasten-search)
      ("e" org-zettelkasten-list-external-refs)

      ("g" org-zettelkasten-grab-ref)
      ("u" org-zettelkasten-use-ref)
      ("p" org-zettelkasten-publish-to-blog))))

(use-package magit :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

(defvar my/custom-bindings-map (make-sparse-keymap)
  "A keymap for custom bindings.")

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  :init-value t
  :lighter nil
  :keymap my/custom-bindings-map
  :group 'my-custom-group
  :global nil)

(define-key my/custom-bindings-map (kbd "<f2>")    #'hydra-zoom/body)
(define-key my/custom-bindings-map (kbd "C-c a")   #'org-agenda)
(define-key my/custom-bindings-map (kbd "C-c c")   #'org-capture)
(define-key org-recur-mode-map     (kbd "C-c d")   #'org-recur-finish)
(define-key my/custom-bindings-map (kbd "C-c l")   #'org-store-link)
(define-key my/custom-bindings-map (kbd "C-c m")   #'magit-status)
(define-key projectile-mode-map    (kbd "C-c p")   #'projectile-command-map)
(define-key my/custom-bindings-map (kbd "C-c s")   #'ispell-word)
(define-key my/custom-bindings-map (kbd "C-c t")   (lambda () (interactive) (org-agenda nil "n")))
(define-key my/custom-bindings-map (kbd "C-c v")   #'ivy-push-view)
(define-key my/custom-bindings-map (kbd "C-c V")   #'ivy-pop-view)
(define-key my/custom-bindings-map (kbd "C-c z")   #'hydra-zettelkasten/body)
(define-key my/custom-bindings-map (kbd "C-x b")   #'counsel-switch-buffer)
(define-key my/custom-bindings-map (kbd "C-x k")   #'my/kill-this-buffer-unless-scratch)
(define-key my/custom-bindings-map (kbd "C-x C-b") #'list-buffers)
(define-key my/custom-bindings-map (kbd "C-x C-f") 'counsel-find-file)
(define-key my/custom-bindings-map (kbd "C-x |")   #'my/toggle-window-split)
(define-key org-mode-map           (kbd "C-c C-o") #'org-open-maybe)
(define-key my/custom-bindings-map (kbd "C-s") 'swiper-isearch)
(define-key evil-window-map        (kbd "C-w")     #'hydra-window/body)
(define-key my/custom-bindings-map (kbd "M-g r")   #'git-gutter:update-all-windows)
(define-key my/custom-bindings-map (kbd "M-s")     #'avy-goto-word-1)
(define-key my/custom-bindings-map (kbd "M-x") #'counsel-M-x)
(define-key my/custom-bindings-map (kbd "M-y") 'counsel-yank-pop)

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)
