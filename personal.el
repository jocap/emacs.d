; John's Personal Preferences
; a.k.a. jocap

;; Quick window switching [jocap]

(global-set-key (kbd "M-/") 'next-multiframe-window)
(global-set-key (kbd "M-?") 'previous-multiframe-window)

;; Open line above and below [Emacs Redux]

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
