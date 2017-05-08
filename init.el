;; =============================================================================
;;                          init.el by John Ankarström
;; =============================================================================

;; 1. Initialization {{{

;; }}}

;; 2. Packages {{{

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 1)))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; if you use :diminish
(require 'bind-key) ; if you use any :bind variant

(use-package wrap-region
  :config (wrap-region-mode t))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package expand-region
  :bind (("C-' r"  . er/expand-region)
         ("C-' w"  . er/mark-word)
         ("C-' '"  . er/mark-inside-quotes)
         ("C-' \"" . er/mark-outside-quotes)
         ("C-' p"  . er/mark-inside-pairs)
         ("C-' P"  . er/mark-outside-pairs)
         ("C-' c"  . er/mark-comment)
         ("C-' t"  . er/mark-inner-tag)
         ("C-' T"  . er/mark-outer-tag)
         ("C-' f"  . er/mark-defun)))

(use-package multiple-cursors
  :bind (("C-c c" . mc/edit-lines)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c ?" . mc/mark-all-like-this))
  :config (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))

(use-package visual-regexp
  :defer) ; prevent loading this package before visual-regexp-steroids!

(use-package visual-regexp-steroids
  :ensure pcre2el ; much faster than Python
  :demand ; load this package immediately, regardless of :bind
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward))
  :config (setq vr/engine 'pcre2el))

;; for more information on how to load visual-regexp-steroids with use-package,
;; see workaround by alamaison @ GitHub at the following link:
;; https://github.com/benma/visual-regexp-steroids.el/issues/16#issue-123951566

(use-package paredit
  :config
  (autoload 'enable-paredit-mode
    "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; re-map M-r, overriden by paredit-raise-sexp
  :bind ("M-R" . move-to-window-line-top-bottom))

(use-package paredit-everywhere
  :ensure paredit
  :config (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package origami
  :config
  (global-origami-mode t)
  (setq-local origami-fold-style 'triple-braces)

  (defun custom-origami-toggle-node () ; (courtesy of /u/Eldrik @ reddit)
    (interactive)
    (save-excursion ; leave point where it is
      (goto-char (point-at-eol)) ; then go to the end of line
      (origami-toggle-node (current-buffer) (point)))) ; and try to fold

  (defun traverse-folds (times &optional beginning)
    "Traverses through folds as many times as ordered by argument.
  A negative argument makes it traverse backwards."

    (unless beginning (setq beginning (point)))
    (if (> times 0)
        (progn
          (move-end-of-line nil)
          (fset 'fun 'origami-forward-fold))
      (progn
        (move-beginning-of-line nil)
        (fset 'fun 'origami-previous-fold)))
    (dotimes (i (abs times))
      (condition-case err
          (fun (current-buffer) (point))
        (error (message "Fold not found: %s" err))))
    (set-mark beginning)
    (deactivate-mark))

  (defun next-fold (times)
    "Jumps to the beginning of the next fold (or previous, on
  negative argument)."

    (interactive "P")
    (unless times (setq times 1))
    (traverse-folds times))

  (defun previous-fold (times)
    "Jumps to the beginning of the previous fold, as many times as
  ordered by argument."

    (interactive "P")
    (unless times (setq times 1))
    (next-fold (* times -1)))

  (defun goto-fold (number)
    "Jumps to fold # (provided by argument) in file."

    (interactive "P")
    (unless number (setq number
                         (string-to-number (read-string "Jump to fold: "))))
    (setq beginning (point))
    (if (equal number 0) (setq number 1))
    (if (> number 0)
        (goto-char (point-min))
      (goto-char (point-max)))
    (traverse-folds number beginning))

  :bind (("M-Z"     . custom-origami-toggle-node)
         ("C-M-z"   . origami-toggle-all-nodes)
         ("C-c C-z" . goto-fold)
         ("C-c C-n" . next-fold)
         ("C-c C-p" . previous-fold)))

(use-package iy-go-to-char
  :bind (("M-m" . iy-go-to-char)
         ("M-M" . iy-go-to-char-backward)
         ("C-." . iy-go-to-char-continue)
         ("C-," . iy-go-to-char-continue-backward)))

(use-package magit
  :bind (("C-c g"   . magit-status)
         ("C-c M-g" . magit-commit)))

(use-package fill-column-indicator
  :bind ("C-c i" . fci-mode))

(use-package avy
  :commands avy-isearch
  :init (global-set-key (kbd "M-'") nil) ; reset M-'
  :config (define-key isearch-mode-map (kbd "M-'") 'avy-isearch)
  :bind (("C-M-'" . abbrev-prefix-mark) ; re-bind default M-'
         ("M-' l" . avy-goto-line)
         ("M-' s" . avy-goto-char-timer)
         ("M-' w" . avy-goto-word-1)))

(use-package ace-link
  :ensure avy
  :config
  (ace-link-setup-default))

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :demand
  :init
  ;; Open agenda in split window at launch
  (add-hook 'after-init-hook (lambda ()
                               (org-agenda-list)
                               (other-window 1)))
  :config
  ;; Make ' work in inline code
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components) ; reload setting

  ;; Make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Remove keybindings that I already use
  (define-key org-mode-map (kbd "C-'") nil)
  (define-key org-mode-map (kbd "C-c C-m") nil)

  (defun org-make-wiktionary-link (string &optional from to)
    "Wraps the word at point or selected word in a Wiktionary link to the word."

    ;; (see http://ergoemacs.org/emacs/elisp_command_working_on_string_or_region.html)
    (interactive
     (if (use-region-p)
         (list nil (region-beginning) (region-end))
       (let ((bds (bounds-of-thing-at-point 'word)) )
         (list nil (car bds) (cdr bds)))))

    (setq wiktionary-language 'russian)

    (let* ((input  (or string (buffer-substring-no-properties from to)))
           (output (concat "[[https://en.wiktionary.org/wiki/"
                           (org-link-escape (downcase input))
                           "#"
                           (capitalize (symbol-name wiktionary-language))
                           "]["
                           input
                           "]]")))
      (delete-region from to)
      (goto-char from)
      (insert output)))

  (defun org-custom-beginning-of-line (original-function &optional n)
    "The exact same function as `org-custom-beginning-of-line',
but with one exception: instead of calling `beginning-of-line'
twice, it calls `smarter-beginning-of-line' once."
    (interactive "^p")
    (let ((origin (point))
          (special (pcase org-special-ctrl-a/e
                     (`(,C-a . ,_) C-a) (_ org-special-ctrl-a/e)))
          deactivate-mark)
      ;; First move to a visible line.
      (if (bound-and-true-p visual-line-mode)
          (beginning-of-visual-line n)
        (smarter-move-beginning-of-line n))
      (cond
       ;; No special behavior.  Point is already at the beginning of
       ;; a line, logical or visual.
       ((not special))
       ;; `beginning-of-visual-line' left point before logical beginning
       ;; of line: point is at the beginning of a visual line.  Bail
       ;; out.
       ((and (bound-and-true-p visual-line-mode) (not (bolp))))
       ((let ((case-fold-search nil)) (looking-at org-complex-heading-regexp))
        ;; At a headline, special position is before the title, but
        ;; after any TODO keyword or priority cookie.
        (let ((refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
                           (line-end-position)))
              (bol (point)))
          (if (eq special 'reversed)
              (when (and (= origin bol) (eq last-command this-command))
                (goto-char refpos))
            (when (or (> origin refpos) (= origin bol))
              (goto-char refpos)))))
       ((and (looking-at org-list-full-item-re)
             (memq (org-element-type (save-match-data (org-element-at-point)))
                   '(item plain-list)))
        ;; Set special position at first white space character after
        ;; bullet, and check-box, if any.
        (let ((after-bullet
               (let ((box (match-end 3)))
                 (cond ((not box) (match-end 1))
                       ((eq (char-after box) ?\s) (1+ box))
                       (t box)))))
          (if (eq special 'reversed)
              (when (and (= (point) origin) (eq last-command this-command))
                (goto-char after-bullet))
            (when (or (> origin after-bullet) (= (point) origin))
              (goto-char after-bullet)))))
       ;; No special context.  Point is already at beginning of line.
       (t nil))))

  (advice-add 'org-beginning-of-line :around #'org-custom-beginning-of-line)

  (add-hook 'org-mode-hook 'swedish-mode) ;; Swedish letters

  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-iswitchb))
  :bind (:map org-mode-map
              ("<C-M-return>" . smart-open-line)
              ("C-c L"        . org-make-wiktionary-link)))

(use-package helm
  :commands helm-command-prefix
  :bind (("M-x"     . helm-M-x)
         ("C-c C-m" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list))
  :init
  (global-set-key (kbd "C-c C-h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)
  (helm-mode 1)
  :config
  (helm-autoresize-mode 1)
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t ; "fuzzy" matching
        helm-split-window-in-side-p           t ; helm inside current window
        helm-autoresize-max-height           40
        helm-autoresize-min-height           0))

(use-package projectile
  :ensure helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)

  (setq projectile-globally-ignored-directories
        (cl-list* ".cache" ".cargo"
                  projectile-globally-ignored-directories))

  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile) ;; see http://tuhdo.github.io/helm-projectile.html#sec-5
  (helm-projectile-on))

(use-package openwith
  :init
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "mupdf" (file)))))

(use-package nlinum
  :bind (("C-c l" . nlinum-mode)))

(use-package popwin
  :init
  (require 'popwin)
  (popwin-mode 1))

(use-package helm-org-rifle
  :bind (("C-c f" . helm-org-rifle-current-buffer)
         ("C-c F" . helm-org-rifle)))

;; }}}

;; 3. Functions {{{

;; Set color scheme according to daylight

(defun daylight-sets-color ()
  "Sets a light theme for day and a dark theme for night.
  Depends on the script `sun' being found in path."

  (interactive)
  (let ((time (string-to-number (format-time-string "%H.%M"))))
        (if (string-match "not found" (shell-command-to-string "which sun"))
            (if (and (> time 6.00) (< time 18.00)) ; default if `sun' not found
                (load-theme light-theme t)
              (load-theme dark-theme t))
          (let ((sunrise
                 (string-to-number (shell-command-to-string "sun _rise")))
                (sunset
                 (string-to-number (shell-command-to-string "sun _set"))))
            (if (and (> time sunrise) (< time sunset))
                (load-theme light-theme t)
              (load-theme dark-theme t))))))

;; Execute shell commands on parent shell (or any other tty)

(defun tty-shell-command (command &optional terminal &optional return-nil)
  "Executes a command on terminal (default: parent tty of frame).
  Note that it only works in Emacs frames attached to using
  `emacsclient -t'."

  (let ((terminal (or terminal (get-device-terminal nil))) ; tty of frame
        (tty (terminal-name terminal)))
    (progn (setq output (shell-command (concat
                                        command
                                        " >"
                                        tty
                                        " 2>&1")))
      (if return-nil nil output))))

(defun tty-set-name (name &optional terminal)
  "Sets the title of the tty in which the current frame is
  open (or the tty provided by argument)."

  (tty-shell-command (concat
                      "echo -ne \"\033]0;"
                      name
                      "\007\"")
                     terminal) t) ; return-nil = t -> return nil no matter what

;; Desktop saving and loading

(defun init-desktop (&optional arg)
  "Load the desktop (if C-u is provided) and enable autosaving."

  (interactive "p")
  (if current-prefix-arg (desktop-read))
  (desktop-save-mode 1)
  (message "Desktop-Save mode enabled"))

;; Custom Helm buffers

;; (defvar my/helm-source-spotify
;;   '((name . "Spotify")
;;     (:candidates "Song 1" "Song 2")))

;; (defun my/helm-spotify-remote ()
;;   (interactive)
;;   (helm :sources '(my/helm-source-spotify)
;;         :buffer "*helm-spotify-remote*"))

;; Various functions

(defun smart-open-line () ; (courtesy of Emacs Redux)
  "Insert an empty line after the current line.
  Position the cursor at beginning, according to current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
  Position the cursor at beginning, according to current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun comment-dwim-line (&optional arg) ; (courtesy of Jason Viers @ SE)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we
  are not at the end of the line, then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts
  comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(defun mark-line ()
  "Marks the current line."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark)
  (setq mark-active t)
  (move-end-of-line nil)
  (forward-char))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg
                (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end
              (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun smarter-move-beginning-of-line (&optional &rest args)
  "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character
  and the beginning of the line.
  If ARG is not nil or 1, move forward ARG - 1 lines first. If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (let ((arg (or (prefix-numeric-value current-prefix-arg) 1)))
    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-to-column 0))))) ; based on function from Emacs Redux

;; }}}

;; 4. Preferences {{{

(server-start) ; use emacs as a server

(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; Themes

(setq light-theme 'solarized-light)
(setq dark-theme 'gruvbox)

(defun theme-do (theme)
  "Actions to perform when `theme' loads."

  (cl-case theme
    ('gruvbox
     (custom-theme-set-faces
      'gruvbox ; fix hard-to-see org-mode colors
      '(org-verbatim ((t (:foreground "DarkGray"))))
      '(org-document-info-keyword ((t (:foreground "DarkGoldenrod"))))))

    ('tango
     (custom-theme-set-faces
      'tango
      '(hl-line ((t (:background "#dddddd"))))))

    ('tango-dark
     (custom-theme-set-faces
      'tango-dark ; fix crazy hl-line (bright yellow per default!)
      '(hl-line ((t (:background "#444444"))))))

    ('eziam-light
     (custom-theme-set-faces
      'eziam-light
      '(cursor ((t (:background "#333333")))) ; lighter cursor
      '(helm-match ((t (:background "#e4e499")))) ; clearer helm match
      '(show-paren-match ((t (:background "#bbbbbb"))))) ; clearer paren match
     ;; Dynamically fading rainbow-delimiters (from black to light gray)
     (cl-loop for n in (number-sequence 1 9)
              do (let ((face-name (concat
                                   "rainbow-delimiters-depth-"
                                   (number-to-string n)
                                   "-face"))
                       (color (color-lighten-name
                               (face-attribute 'default :foreground)
                               (* n 8))))
                   (custom-theme-set-faces
                    'eziam-light
                    (list (intern face-name) `((t (:foreground ,color))))))))))

(defun theme-do-all ()
  "Actions to perform when any theme loads"

  ;; Dynamic colors

  (require 'color)
  (let* ((bg
          (alist-get 'background-mode (frame-parameters)))
         (intensify
          (if (eq bg 'dark) 'color-darken-name 'color-lighten-name))
         (anti-intensify
          (if (eq bg 'dark) 'color-lighten-name 'color-darken-name)))

    ;; * fci-rule-color -> desaturate, anti-intensity
    (setq fci-rule-color (color-desaturate-name
                          (funcall anti-intensify
                                   (face-attribute 'default :background) 15) 50))

    ;; * org-block-background -> desaturate, darken
    (set-face-attribute 'org-block nil
                        :background (color-desaturate-name
                                     (color-darken-name
                                      (face-attribute 'default :background) 3) 20)
                        :foreground (face-attribute 'default :foreground))

    ;; * org-block-begin-line, *org-block-end-line
    (cl-loop
     for face in '(org-block-begin-line org-block-end-line)
     do (set-face-attribute
         face nil
         :background (color-desaturate-name
                      (color-darken-name
                       (face-attribute 'default :background) 15) 50)
         :foreground (color-desaturate-name
                      (funcall intensify
                               (face-attribute 'default :foreground) 20) 90)
         :weight (face-attribute 'default :weight)
         :slant (face-attribute 'default :slant))))

  ;; - Reset fci-mode
  (let ((inhibit-message t))
    (call-interactively 'fci-mode)
    (call-interactively 'fci-mode)))

;; - Disable previous theme when enabling new theme
(add-hook 'after-init-hook
          (lambda () (defadvice load-theme
                         (before theme-dont-propagate activate)
                       (mapcar #'disable-theme custom-enabled-themes))))

;; - Dynamic settings for different themes
(advice-add 'load-theme :after (lambda (theme &optional rest ...)
                                 (theme-do theme)
                                 (theme-do-all)))

;; - Set theme according to daylight
(add-hook 'after-init-hook 'daylight-sets-color)

;; Dynamic cursor (focused -> bar, unfocused -> block)

(setq default-cursor-type cursor-type)

(defun cursor-focused ()
  (setq-local cursor-type default-cursor-type))
(defun cursor-unfocused ()
  (setq-local cursor-type 'block))

(add-hook 'post-command-hook 'cursor-focused)
;; Using post-command-hook because setting the cursor type is such a simple
;; action, and because select-window is called surprisingly often, and often
;; without the suitable `norecord' argument ...

(add-hook 'window-focus-out-hook  'cursor-unfocused)
(add-hook 'before-minibuffer-hook 'cursor-unfocused)
(add-hook 'before-helm-hook       'cursor-unfocused)

;; Directories

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq emacs-state-directory (expand-file-name "state/" user-emacs-directory))
(defun state-dir (file)
  (concat emacs-state-directory file))

(setq auto-save-list-file-prefix (state-dir "auto-save-list/.saves~"))
(setq save-place-file (state-dir "save-place"))
(setq recentf-save-file (state-dir "recentf"))
(setq ido-save-directory-list-file (state-dir "ido.last"))
(setq backup-directory-alist
      `((".*" . ,(state-dir "saves"))))

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory (state-dir
                                        "tramp/auto-save/"))
(setq tramp-persistency-file-name (state-dir
                                          "tramp/persistency.el"))

;; - Projectile
(setq projectile-cache-file (concat emacs-state-directory
                                    "projectile/cache.el"))
(setq projectile-known-projects-file
      (concat emacs-state-directory
              "projectile/known-projects.el"))

;; Desktop
(setq desktop-dirname             (concat emacs-state-directory "desktop/")
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t)

;; Spelling

(require 'ispell)
(add-to-list 'ispell-dictionary-alist
             '("swedish-hunspell"
               "[[:alpha:]]"
               "[^[:alpha:]]"
               "[']"
               t
               ("-d" "sv_SE")
               nil
               utf-8))

(add-to-list 'ispell-dictionary-alist
             '("english-hunspell"
               "[[:alpha:]]"
               "[^[:alpha:]]"
               "[']"
               t
               ("-d" "en_US")
               nil
               utf-8))
(setq ispell-program-name "hunspell"
      ispell-dictionary   "swedish-hunspell")

;; Shebang mode detection
(add-to-list 'interpreter-mode-alist
             '("python3" . python-mode))

;; Enabling disabled commands
(defadvice en/disable-command (around put-in-custom-file activate)
  "Put declarations in `custom-file'."
  (let ((user-init-file (concat user-emacs-directory ".commands")))
    ad-do-it))
(load-file (concat user-emacs-directory ".commands"))

;; Change cursor to block on suspend, and back to ibeam on resume
(add-hook 'suspend-tty-functions
          (lambda (terminal)
            (tty-shell-command "echo -ne \"\e[2 q\"" terminal)))
(add-hook 'resume-tty-functions
          (lambda (terminal)
            (tty-shell-command "echo -ne \"\e[6 q\"" terminal)))

;; }}}

;; 5. Keybindings {{{

(windmove-default-keybindings)

(global-set-key (kbd "M-<f1>") 'menu-bar-mode)

(global-set-key (kbd "C-c D") 'init-desktop)
(global-set-key (kbd "M-n") (lambda (n) (interactive "p") (scroll-up n)))
(global-set-key (kbd "M-p") (lambda (n) (interactive "p") (scroll-down n)))
(global-set-key (kbd "M-RET") 'smart-open-line)
(global-set-key (kbd "M-o") 'smart-open-line-above)
(global-set-key (kbd "C-;") 'comment-dwim-line)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Insert combining acute accent
(global-set-key (kbd "C-c 8 '") (lambda () (interactive) (insert-char 769)))

;; }}}

;; 6. Mode configuration {{{

(electric-pair-mode 1) ; auto-insert matching pairs
(menu-bar-mode -1)     ; disable menu bar
;; (global-hl-line-mode)  ; highlight current line
(save-place-mode 1)    ; save cursor position
(xterm-mouse-mode t)   ; use mouse (somewhat) in terminal
(tool-bar-mode -1)     ; disable gui toolbar

;; auto-fill-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; python-mode
(defun shell-compile () ; (courtesy of djangoliv @ stack interchange)
  (interactive)
  (shell-command (concat "python " (buffer-file-name)))
  (if (<= (* 2 (window-height)) (frame-height))
      (enlarge-window 20)
    (/ (frame-height) 2)))
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-c C-c") 'shell-compile)))


;; (La)TeX-mode
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
(defun my-latex-setup (&optional args)
  (defun start-update-viewer ()
    "Starts/updates PDF viewer."
    (interactive)
    (if (string-match "no process found"
                      (shell-command-to-string "killall -HUP mupdf-x11"))
        (start-process-shell-command
         "mupdf"          ; process name
         "mupdf"          ; process buffer
         (concat "mupdf " ; shell command
                 (expand-file-name
                  (concat "output/"
                          (file-name-base (buffer-file-name))
                          ".pdf"))))))
  (define-key LaTeX-mode-map (kbd "C-c C-u") 'start-update-viewer)

  (defun save-run ()
    "Saves the document and processes it."
    (interactive)
    (save-buffer)
    (TeX-command-run-all nil))

  (defun latex-word-count () ; (courtesy of Nicholas Riley @ SE)
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count)

  (defun latex-write-word-count ()
    "Writes the word count to count.txt (if it exists)."
    (interactive)
    (shell-command (concat "texcount -brief "
                    (shell-quote-argument buffer-file-name)
                    " | sed -e 's/+.*//' > count.txt; cat count.txt")))
  (define-key LaTeX-mode-map (kbd "C-c M-w") 'latex-write-word-count))
(add-hook 'LaTeX-mode-hook 'my-latex-setup t)

;; }}}

;; 7. Custom hooks {{{

;; window-focus-out-hook, window-focus-in-hook

(defun run-window-focus-out-hook (window &optional norecord)
  (unless norecord
    (run-hooks 'window-focus-out-hook)))
(defun run-window-focus-in-hook (window &optional norecord)
  (unless norecord
    (run-hooks 'window-focus-in-hook)))

(advice-add 'select-window :before 'run-window-focus-out-hook)
(advice-add 'select-window :after 'run-window-focus-in-hook)

;; NOTE: This doesn't always play nice with magit. For example, select-window
;; seems to be run when opening the commit message buffer, but *not* when
;; returning to the magit status buffer. I'm not quite sure why, but I suppose I
;; could add an exception for it. I'd have to look at the magit source. Perhaps
;; I could just run a function upon switch-to-buffer that checks whether the
;; current-window is different from the previous-current-window (saved in a
;; variable); that might be the most simple solution, similar to what hl-line
;; does, but as I've said before, more efficient than attaching everything to
;; post-command-hook ...

;; TODO: Add exception for magit buffer switching.

;; before-minibuffer-hook, after-minibuffer-hook

(defun run-before-minibuffer-hook (&optional &rest args)
  (run-hooks 'before-minibuffer-hook)
  (add-hook 'post-command-hook 'run-after-minibuffer-hook))
(defun run-after-minibuffer-hook (&optional &rest args)
  (unless (minibufferp)
    (run-hooks 'after-minibuffer-hook)
    (remove-hook 'post-command-hook 'run-after-minibuffer-hook)))

(advice-add 'read-from-minibuffer :before 'run-before-minibuffer-hook)
(advice-add 'read-no-blanks-input :before 'run-before-minibuffer-hook)
(advice-add 'read-string          :before 'run-before-minibuffer-hook)

;; before-helm-hook, after-helm-hook

(defun run-before-helm-hook (&optional &rest args)
  (run-hooks 'before-helm-hook))
(defun run-after-helm-hook (&optional &rest args)
  (run-hooks 'after-helm-hook))

(add-hook 'helm-before-initialize-hook 'run-before-helm-hook)
(add-hook 'helm-exit-minibuffer-hook   'run-after-helm-hook)
(advice-add 'helm-keyboard-quit :after 'run-after-helm-hook)

;; }}}

;; 8. Custom modes {{{

;; Swedish letters

;; Based on work by Moritz Ulrich <ulrich.moritz@googlemail.com>
;; Published under GNU General Public License

(defvar swedish-mode-map (make-keymap) "Swedish mode keymap.")

(define-key swedish-mode-map (kbd "s-[")  (lambda () (interactive) (insert ?å)))
(define-key swedish-mode-map (kbd "s-'")  (lambda () (interactive) (insert ?ä)))
(define-key swedish-mode-map (kbd "s-;")  (lambda () (interactive) (insert ?ö)))
(define-key swedish-mode-map (kbd "s-{")  (lambda () (interactive) (insert ?Å)))
(define-key swedish-mode-map (kbd "s-\"") (lambda () (interactive) (insert ?Ä)))
(define-key swedish-mode-map (kbd "s-:")  (lambda () (interactive) (insert ?Ö)))

(define-minor-mode swedish-mode
  "A mode for conveniently using Swedish letters in Emacs."
  nil
  :lighter " åäö"
  swedish-mode-map)

(provide 'swedish-mode)

;; }}}

;; 9. Customize {{{

(load custom-file)

;; }}}
