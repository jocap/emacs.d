; John's Personal Preferences
; a.k.a. jocap

;; Miscellaneous preferences

(load-theme 'wombat) ; an awesome theme
(electric-pair-mode 1) ; lisp auto-insertion of parentheses
(setq kill-whole-line t) ; C-k kills newline as well
(setq-default indent-tabs-mode nil) ; don't mix spaces and tabs
(setq tab-width 4)
(global-linum-mode) ; line numbers
(setq linum-format "%02d | ")

;; Things that make my life easier

(defun mark-line ()
  "Marks the current line."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark)
  (setq mark-active t)
  (move-end-of-line nil)
  (forward-char))

(global-set-key (kbd "M-]") 'mark-line)
(global-set-key (kbd "ESC <f3>") 'kmacro-start-macro-or-insert-counter) ; for some reason, <F3> outputs "R" when using tmux
(global-set-key (kbd "ESC <f4>") 'kmacro-end-or-call-macro) ; same problem here, but with "S"
(global-set-key (kbd "ESC <f5>") 'toggle-truncate-lines)

;; Markdown preview (requires my markdown-preview script and its dependencies)

; currently not functional
(defun md-preview ()
  "Converts the current file from Markdown to HTML and opens the result in Google Chrome."
  (interactive)
  (start-process "mdpreview" (get-buffer-create "*md-preview-buffer*") "markdown-preview" (file-name-directory load-file-name)))

(global-set-key (kbd "M-p") 'md-preview)

;; Quick window switching

(global-set-key (kbd "M-/") 'next-multiframe-window)
(global-set-key (kbd "M-?") 'previous-multiframe-window)

;; Open-line above and below [Emacs Redux]

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)
