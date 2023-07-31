;;; -*- lexical-binding: t; byte-compile-warnings: (not docstrings); -*-

; automatically compile Emacs Lisp libraries
(use-package auto-compile
  :ensure t)
; load elisp libraries while Emacs is idle
(use-package idle-require
  :ensure t)
; Try out Emacs packages
(use-package try
  :ensure t)
(use-package which-key
  :ensure t)
(use-package exec-path-from-shell
  :ensure t)
; Modular text completion framework
(use-package company
  :ensure t)
; Emacs support library for PDF files
(use-package pdf-tools
  :ensure t)
; minor mode for editing parentheses
(use-package paredit
  :ensure t)
(use-package focus                ; Dim color of text in surrounding sections
  :ensure t)
; Diminished modes from modeline
(use-package diminish
  :ensure t)
; Increase selected region by semantic units
(use-package expand-region
  :ensure t)
(use-package solarized-theme
  :ensure t)
(use-package define-word          ; display the definition of word at point
  :ensure t)
(use-package avy
  :ensure t)
(use-package smooth-scrolling
  :ensure t)
(use-package git-gutter-fringe
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package auto-compile
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(when (memq window-system '(mac ns))
  (defvar select-enable-clipboard t)
  (defvar mac-option-key-is-meta nil)
  (defvar mac-command-key-is-meta t)
  (defvar mac-command-modifier 'meta)
  (defvar mac-option-modifier nil)
  (exec-path-from-shell-initialize)
  (grep-compute-defaults)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode 1)))

(when (eq window-system 'gnu/linux)
  (exec-path-from-shell-initialize))

(when (eq window-system 'w32)
  (grep-compute-defaults))

(require 'idle-require)             ; Need in order to use idle-require

(dolist (feature
         '(auto-compile             ; auto-compile .el files
           ox-latex                 ; the latex-exporter (from org)
           ox-md                    ; Markdown exporter (from org)
           recentf                  ; recently opened files
           tex-mode))               ; TeX, LaTeX, and SliTeX mode commands
  (idle-require feature))

(setq idle-require-idle-delay 5)
(idle-require-mode 1)

(load-file "~/.emacs.d/settings.el")

(setq auto-revert-interval 1            ; Refresh buffers fast
      custom-file (make-temp-file "")   ; Discard customization's
      default-input-method "TeX"        ; Use TeX when toggling input method
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      sentence-end-double-space nil)    ; No double space
;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(setq-default indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil)       ; Split verticly by default

(fset 'yes-or-no-p 'y-or-n-p)

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

(put 'narrow-to-region 'disabled nil)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-hook 'write-file-functions 'delete-trailing-whitespace)

(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           menu-bar-mode                ; No menubar
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))

(dolist (mode
         '(abbrev-mode                  ; E.g. sopl -> System.out.println
           column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           dirtrack-mode                ; directory tracking in *shell*
           global-company-mode          ; Auto-completion everywhere
           global-git-gutter-mode       ; Show changes latest commit
           recentf-mode                 ; Recently opened files
           show-paren-mode              ; Highlight matching parentheses
           smooth-scrolling-mode
           which-key-mode))             ; Available keybindings in popup
  (funcall mode 1))

(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(require 'color-theme-sanityinc-tomorrow)
(if (display-graphic-p)
  (color-theme-sanityinc-tomorrow-night)
  (load-theme 'solarized t))

(cond ((member "DejaVu Sans Mono" (font-family-list))
       (set-face-attribute 'default nil :font "DejaVu Sans Mono")
       (set-frame-font "DejaVu Sans Mono" nil t)))

(use-package unicode-fonts :ensure t)
;(unicode-fonts-setup)

;; Use this font if the current font can't render a symbol
(set-fontset-font "fontset-default" 'unicode "DejaVu Sans")

(prefer-coding-system 'utf-8)

(defmacro safe-diminish (file mode &optional new-name)
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(safe-diminish "eldoc" 'eldoc-mode)
(safe-diminish "flyspell" 'flyspell-mode)
(safe-diminish "projectile" 'projectile-mode)
(safe-diminish "paredit" 'paredit-mode "()")

(require 'git-gutter-fringe)

(dolist (p '((git-gutter:added    . "#0c0")
             (git-gutter:deleted  . "#c00")
             (git-gutter:modified . "#c0c")))
  (set-face-foreground (car p) (cdr p))
  (set-face-background (car p) (cdr p)))

(add-hook 'pdf-tools-enabled-hook 'auto-revert-mode)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))

(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

(when browser-path
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program browser-path))

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(defun always-true (&rest _args) 1)

(defun mk-y-or-n-p-always-true (old &rest args)
    (progn
      (advice-add 'y-or-n-p :override #'always-true)
      (let ((result (apply old args)))
        (advice-remove 'y-or-n-p #'always-true)
        result)))

(defun kill-this-buffer-unless-scratch ()
  "Works like `kill-this-buffer' unless the current buffer is the
*scratch* buffer. In witch case the buffer content is deleted and
the buffer is buried."
  (interactive)
  (if (not (string= (buffer-name) "*scratch*"))
      (kill-this-buffer)
    (delete-region (point-min) (point-max))
    (switch-to-buffer (other-buffer))
    (bury-buffer "*scratch*")))

(define-key custom-bindings-map (kbd "C-x k") 'kill-this-buffer-unless-scratch)

(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun insert-current-date (&optional omit-day-of-week-p)
   "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
   (interactive "P*")
   (insert (calendar-date-string (calendar-current-date) nil
                                 omit-day-of-week-p)))

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-time ()
  "Insert the current time."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(defun current-date ()
  (calendar-date-string
    (calendar-current-date)
    nil))

(defun start-of-week ()
  (calendar-date-string
   (calendar-gregorian-from-absolute
    (calendar-dayname-on-or-before
      0 ; Sunday
      (calendar-absolute-from-gregorian (calendar-current-date))))))

(defun current-month ()
  (let ((dt (calendar-current-date)))
    (format "%s %s" (calendar-month-name (nth 0 dt)) (nth 2 dt))))

(defun linear-regression (xs ys)
  (let* ((sum (lambda (items) (seq-reduce #'+ items 0)))
         (avg-y (/ (funcall sum ys) (float (length ys))))
         (avg-x (/ (funcall sum xs) (float (length xs))))
         (prods (funcall sum (zipWith (lambda (x y)
                                        (* (- x avg-x) (- y avg-y)))
                                      xs ys)))
         (sqrds (funcall sum (seq-map (lambda (x) (expt (- x avg-x) 2)) xs))))
    (/ prods (float sqrds))))

(defun single-regressor (points)
  (linear-regression (seq-map-indexed (lambda (_x i) i) points) points))

(defun zipWith (f xs ys)
  (if (or (eq xs nil) (eq ys nil))
     '()
    (cons (funcall f (car xs) (car ys)) (zipWith f (cdr xs) (cdr ys)))))
(defun zip (xs ys) (zipWith #'list xs ys))
(defun pairs (xs) (zip xs (cdr xs)))

(defun jump-to-symbol-internal (&optional backwardp)
  "Jumps to the next symbol near the point if such a symbol
exists. If BACKWARDP is non-nil it jumps backward."
  (let* ((point (point))
         (bounds (find-tag-default-bounds))
         (beg (car bounds)) (end (cdr bounds))
         (str (isearch-symbol-regexp (find-tag-default)))
         (search (if backwardp 'search-backward-regexp
                   'search-forward-regexp)))
    (goto-char (if backwardp beg end))
    (funcall search str nil t)
    (cond ((<= beg (point) end) (goto-char point))
          (backwardp (forward-char (- point beg)))
          (t  (backward-char (- end point))))))

(defun jump-to-previous-like-this ()
  "Jumps to the previous occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal t))

(defun jump-to-next-like-this ()
  "Jumps to the next occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal))

(define-key custom-bindings-map (kbd "M-,")   'jump-to-previous-like-this)
(define-key custom-bindings-map (kbd "M-.")   'jump-to-next-like-this)

(defun find-first-non-ascii-char ()
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
      (message "No non-ascii characters."))))

(defun sort-words-in-region (start end)
  "Sort the words in a given region (START and END) and return them as a list."
   (sort (split-string (buffer-substring-no-properties start end)) #'string<))

(defun sort-words-sorted (start end)
  "Sort the words in a given region (START and END) and return them as a string."
  (mapconcat 'identity (sort-words-in-region start end) " "))

;;;###autoload
(defun sort-words (start end)
  "Sort words in region alphabetically.
Then insert them replacing the existing region.
START and END are boundries of the selected region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((words (sort-words-sorted (point-min) (point-max))))
        (delete-region (point-min) (point-max))
        (goto-char (point-min))
        (insert words)))))

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(defun update-init-el ()
  (interactive)
  (package-refresh-contents)
  ;; Open the configuration
  (find-file (concat user-emacs-directory "init.org"))
  ;; tangle it
  (org-babel-tangle)
  ;; load it
  (load-file (concat user-emacs-directory "init.el"))
  ;; finally byte-compile it
  (byte-compile-file (concat user-emacs-directory "init.el"))
)

(defun toggle-window-split ()
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

(define-key custom-bindings-map (kbd "C-x |") 'toggle-window-split)

(defun general-workspace ()
 "Set up a general split screen workspace."
 (interactive)
 (delete-other-windows nil)
 (split-window-below nil)
 (split-window-right nil)
 (other-window 1)
 (other-window 1)
 (eshell nil))

 (defun single-window-workspace ()
 "Change to a single window workspace."
 (interactive)
 (delete-other-windows nil))

(defun zettelkasten-workspace ()
  "A workspace for editing the Zettelkasten"
  (interactive)
  (delete-other-windows nil)
  (when (boundp 'zettelkasten-people-path)
    (find-file zettelkasten-people-path))
  (split-window-right 50)
  (other-window 1)
  (when (boundp 'zettelkasten-journal-path)
    (find-file zettelkasten-journal-path))
  (when (get-buffer "*Org Agenda*")
    (split-window-right nil)
    (other-window 1)
    (switch-to-buffer (get-buffer "*Org Agenda*"))))

(define-key custom-bindings-map (kbd "M-s") 'avy-goto-word-1)

(defun calendar-show-week (arg)
  "Displaying week number in calendar-mode."
  (interactive "P")
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute
   'calendar-iso-week-face nil :height 0.7)
  (setq calendar-intermonth-text
        (and arg
             '(propertize
               (format
                "%2d"
                (car (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian
                       (list month day year)))))
               'font-lock-face 'calendar-iso-week-face))))

(calendar-show-week t)

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

(define-key custom-bindings-map (kbd "C->")  'er/expand-region)
(define-key custom-bindings-map (kbd "C-<")  'er/contract-region)

(defun cycle-languages ()
  "Changes the ispell dictionary to the first element in
ISPELL-LANGUAGES, and returns an interactive function that cycles
the languages in ISPELL-LANGUAGES when invoked."
  (lexical-let ((ispell-languages '#1=("american" "norsk" . #1#)))
    (ispell-change-dictionary (car ispell-languages))
    (lambda ()
      (interactive)
      ;; Rotates the languages cycle and changes the ispell dictionary.
      (ispell-change-dictionary
       (car (setq ispell-languages (cdr ispell-languages)))))))

(defadvice turn-on-flyspell (before check nil activate)
  "Turns on flyspell only if a spell-checking tool is installed."
  (when (executable-find ispell-program-name)
    (local-set-key (kbd "C-c l") (cycle-languages))))

(defadvice flyspell-prog-mode (before check nil activate)
  "Turns on flyspell only if a spell-checking tool is installed."
  (when (executable-find ispell-program-name)
    (local-set-key (kbd "C-c l") (cycle-languages))))

(define-key custom-bindings-map (kbd "C-c s") 'ispell-word)

(use-package git-gutter-fringe
  :ensure t)

(define-key custom-bindings-map (kbd "M-g r") #'git-gutter:update-all-windows)

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

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-x b")  #'counsel-switch-buffer)
(global-set-key (kbd "C-x C-b") #'list-buffers)
(global-set-key (kbd "C-x k")  #'kill-buffer*)

(setq ivy-re-builders-alist
      '((swiper-isearch . ivy--regex)
        (counsel-projectile-find-file . ivy--regex)
        (t             . ivy--regex-fuzzy)))

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

(setq ivy-prescient-enable-filtering t)
(setq prescient-filter-method '(literal regexp fuzzy))

(setq ivy-prescient-sort-commands
  '(:not ivy-switch-buffer swiper-isearch))

(use-package hydra
  :ensure t)
(require 'hydra)

(defhydra hydra-zoom nil
"zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out"))

(global-set-key (kbd "<f2>") #'hydra-zoom/body)

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
   ("SPC" nil)
   )

(define-key evil-window-map (kbd "C-w") 'hydra-window/body)

(use-package magit :ensure t)
(define-key custom-bindings-map (kbd "C-c m") 'magit-status)

(setq find-program find-location)

(use-package projectile
  :ensure t)
(use-package counsel-projectile
  :ensure t)

(require 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(counsel-projectile-mode)

(use-package yasnippet
  :ensure t
  :config
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)
)

(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp) (dhall-mode .lsp))
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package flycheck
  :ensure t)
(setq
  lsp-ui-sideline-enable t
  lsp-fly-check-enable t)
(use-package lsp-haskell
  :ensure t
  :config (setf lsp-haskell-server-path "haskell-language-server-wrapper"))

(add-hook 'compilation-filter-hook 'comint-truncate-buffer)

(use-package graphviz-dot-mode
  :ensure t
  :config
   (setq graphviz-dot-indent-width 4))

(use-package haskell-mode
  :ensure t)

(setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "ormolu")

(use-package dhall-mode
  :ensure t
  :config
   (setq
     dhall-format-arguments (\` ("--ascii"))
     dhall-use-header-line nil))

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package elmacro
  :ensure t)
(use-package lispy
  :ensure t)
(use-package evil-lispy
  :ensure t)

(dolist (mode '(ielm-mode
                lisp-mode
                emacs-lisp-mode
                lisp-interaction-mode
                scheme-mode))
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

(use-package lua-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun insert-markdown-inline-math-block ()
  "Inserts an empty math-block if no region is active, otherwise wrap a
math-block around the region."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (body (if (region-active-p) (buffer-substring beg end) "")))
    (when (region-active-p)
      (delete-region beg end))
    (insert (concat "$math$ " body " $/math$"))
    (search-backward " $/math$")))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)
            (local-set-key (kbd "C-c b") 'insert-markdown-inline-math-block)) t)

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
    (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)
    (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)
    (setq org-recur-finish-done t
          org-recur-finish-archive t))

(setq org-modules '(org-habit))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
 )
)

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

(setq org-agenda-custom-commands
  '(("!" "Expired Deadlines" tags-todo
     "+DEADLINE<\"<today>\"/TODO|PROGRESSING"
     nil nil)
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
    ("ORDERD" . (:foreground "orange"))
    ("BOUGHT" . (:foreground "green"))
    ("CANCELED" . (:foreground "grey"))
    ("VERIFIED" . (:foreground "green"))
    ("ANSWERED" . (:foreground "green"))))

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
    (sequence "DEVELOPING(v)" "READY(y)" "PROGRESSING(p)" "HOLD(h)" "|" "FINISHED(f)")
    (sequence "UNKNOWN(u)" "RESEARCHING(r)" "|" "ANSWERED(a)")
    (sequence "BUY(b)" "ORDERED(o)" "|" "BOUGHT(h)")
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

(setq org-log-done 'time)

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
  (:name "Time Sensitive"
     :time-grid t
     :tag "TimeSensitive"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Objectives"
     :tag "Objective")
  (:name "Meals"
     :tag "Meal")
  (:name "Shower"
     :tag "Shower"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Short Term Wellfare"
     :priority "A"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Long Term Wellfare"
     :priority "B"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Validation"
     :priority "C"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Actualization"
     :priority "D"
     :transformer #'my-org-agenda-remove-recur)
  (:name "Reading"
     :priority "E"
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

(defun org-sync-pdf ()
  (interactive)
  (let ((headline (nth 4 (org-heading-components)))
        (pdf (concat (file-name-base (buffer-name)) ".pdf")))
    (when (file-exists-p pdf)
      (find-file-other-window pdf)
      (pdf-links-action-perform
       (cl-find headline (pdf-info-outline pdf)
                :key (lambda (alist) (cdr (assoc 'title alist)))
                :test 'string-equal)))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") 'org-sync-pdf))

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
(setq org-latex-pdf-process
    '("bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""
      "bash -c \"bibtex %b\""
      "bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""
      "bash -c \"pdflatex -interaction nonstopmode -output-directory %o %f\""))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)

;; (require 'org-ref)

(define-key custom-bindings-map (kbd "C-c c")       'org-capture)
(define-key custom-bindings-map (kbd "C-c t")
  (lambda () (interactive) (org-agenda nil "n")))
(define-key custom-bindings-map (kbd "C-c l")       'org-store-link)
(define-key custom-bindings-map (kbd "C-c a")       'org-agenda)
;; Redefine file opening without clobbering universal argumnet
(define-key org-mode-map "\C-c\C-o" 'org-open-maybe)

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
  ("p" org-zettelkasten-publish-to-blog)
)

(global-set-key (kbd "C-c z") #'hydra-zettelkasten/body)

(use-package powershell-mode
  :ensure t
  :config
   (setq powershell-indent 2))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)

(setq gc-cons-threshold 800000)
