; ==============================================================================
;                           init.el by John Ankarström
; ==============================================================================

;; 1. Packages {{{

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; }}}

;; 2. Functions {{{

;; Set color scheme according to daylight

(defun daylight-sets-color ()
  "Sets a light theme for day and a dark theme for night.
  Depends on the script 'sun' being found in path."

  (interactive)
  (let ((time (string-to-number (format-time-string "%H.%M"))))
        (if (string-match "not found" (shell-command-to-string "which sun"))
            (if (and (> time 6.00) (< time 18.00))
                (load-theme light-theme t)
              (load-theme dark-theme t))
          (let ((sunrise (string-to-number (shell-command-to-string "sun _rise")))
                (sunset (string-to-number (shell-command-to-string "sun _set"))))
            (if (and (> time sunrise) (< time sunset))
                (load-theme light-theme t)
              (load-theme dark-theme t))))))

;; Jump to next fold

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
      (error (message "Fold not found."))))
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

;; Execute shell commands on parent shell (or any other tty)

(defun tty-shell-command (command &optional terminal &optional return-nil)
  "Executes a command on terminal (default: parent tty of frame).
  Note that it only works in Emacs frames attached to using
  emacsclient -t."
  
  (unless terminal (setq terminal (get-device-terminal nil))) ; tty of frame
  (let ((tty (terminal-name terminal)))
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

;; Dynamic settings for light/dark themes

(defun dynamic-load-theme (theme &optional rest ...)
  (if (and (fboundp 'light-theme-do) (equal theme 'solarized-light))
      (light-theme-do))
  (if (and (fboundp 'dark-theme-do) (equal theme 'gruvbox))
      (dark-theme-do)))

;; Various functions

(defun ctrl-e-in-vi (n)
  (interactive "p")
  (scroll-up n))
(defun ctrl-y-in-vi (n)
  (interactive "p")
  (scroll-down n))

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
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
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

(defun smarter-move-beginning-of-line (arg) ; (cy/o Emacs Redux)
  "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character
  and the beginning of the line.
  If ARG is not nil or 1, move forward ARG - 1 lines first. If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; }}}

;; 3. Preferences {{{

(server-start) ; use emacs as a server

;; Directories
(setq custom-file (concat user-emacs-directory "custom.el"))

(setq emacs-state-directory (concat user-emacs-directory "state/"))
(setq save-place-file (concat emacs-state-directory "save-place"))
(setq recentf-save-file (concat emacs-state-directory "recentf"))
(setq ido-save-directory-list-file (concat emacs-state-directory "ido.last"))
(setq backup-directory-alist
      `((".*" . ,(concat emacs-state-directory "saves"))))

;; Shebang mode detection
(add-to-list 'interpreter-mode-alist
             '("python3" . python-mode))

;; GUI
(add-to-list 'default-frame-alist
             '(font . "Fira Mono Medium-10"))

;; Themes

(setq light-theme 'solarized-light)
(setq dark-theme 'gruvbox)

(defun light-theme-do ()
  (setq fci-rule-color "#cccccc"))

(defun dark-theme-do ()
  (setq fci-rule-color "#333333"))

;; - Set theme according to daylight
(daylight-sets-color)

;; - Disable previous theme when enabling new theme
(add-hook 'after-init-hook
          (lambda () (defadvice load-theme 
                         (before theme-dont-propagate activate)
                       (mapcar #'disable-theme custom-enabled-themes))))

;; - Dynamic settings for light/dark themes
(advice-add 'load-theme :after #'dynamic-load-theme)

;; Enabling disabled commands
(defadvice en/disable-command (around put-in-custom-file activate)
  "Put declarations in `custom-file'."
  (let ((user-init-file "/home/john/.emacs.d/.commands"))
    ad-do-it))

;; Change cursor to block on suspend, and back to ibeam on resume
(add-hook 'suspend-tty-functions
          (lambda (terminal)
            (tty-shell-command "echo -ne \"\e[2 q\"" terminal)))
(add-hook 'resume-tty-functions
          (lambda (terminal)
            (tty-shell-command "echo -ne \"\e[6 q\"" terminal)))

;; }}}

;; 4. Keybindings {{{

(global-set-key (kbd "M-<f1>") 'menu-bar-mode)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "C-.") 'iy-go-to-char-continue)
(global-set-key (kbd "C-,") 'iy-go-to-char-continue-backward)

(global-set-key (kbd "C-c C-z") 'goto-fold)
(global-set-key (kbd "C-c C-n") 'next-fold)
(global-set-key (kbd "C-c C-p") 'previous-fold)

(global-set-key (kbd "M-n") 'ctrl-e-in-vi)
(global-set-key (kbd "M-p") 'ctrl-y-in-vi)
(global-set-key (kbd "M-RET") 'smart-open-line)
(global-set-key (kbd "M-o") 'smart-open-line-above)
(global-set-key (kbd "C-;") 'comment-dwim-line)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; }}}

;; 5. Mode configuration {{{

(electric-pair-mode 1)      ; auto-insert matching pairs
(menu-bar-mode -1)          ; disable menu bar
(global-hl-line-mode)       ; highlight current line
(global-linum-mode)         ; line numbers
(save-place-mode 1)         ; save cursor position
(xterm-mouse-mode t)        ; use mouse (somewhat) in terminal
(tool-bar-mode -1)          ; disable gui toolbar

;; fci-mode
(global-set-key (kbd "C-c i") 'fci-mode)

;; wrap-region
(wrap-region-mode t)

;; emacs-dashboard
(dashboard-setup-startup-hook)

;; auctex-latexmk
(auctex-latexmk-setup)

;; ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere 1)

;; flx-ido-mode (fuzzy matching for ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; auto-fill-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; expand-region
(global-set-key (kbd "C-\\") 'er/expand-region)
(global-set-key (kbd "C-' w") 'er/mark-word)
(global-set-key (kbd "C-' '") 'er/mark-inside-quotes)
(global-set-key (kbd "C-' \"") 'er/mark-outside-quotes)
(global-set-key (kbd "C-' c") 'er/mark-comment)
(global-set-key (kbd "C-' t") 'er/mark-inner-tag)
(global-set-key (kbd "C-' T") 'er/mark-outer-tag)
(global-set-key (kbd "C-' f") 'er/mark-defun)

;; multiple-cursors
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ?") 'mc/mark-all-like-this)
(add-hook 'multiple-cursors-hook
          (lambda () ; I'm not sure how, but this is supposed to be good
            (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)))

;; visual-regexp-steroids
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ; C-M-s
;; (NOTE: Make sure to use pcre2el - much faster than Python)

;; paredit

;; - enable automatically
(autoload 'enable-paredit-mode
  "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode) ; paredit-everywhere

;; - keybindings
(global-set-key (kbd "C-h )") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-h }") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-h (") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-h {") 'paredit-backward-barf-sexp)

(add-hook 'paredit-mode-hook ; re-map M-r, overriden by paredit-mode
          (lambda ()
            (local-set-key (kbd "M-R") 'move-to-window-line-top-bottom)))

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

;; origami-mode
(defun custom-origami-toggle-node () ; (courtesy of /u/Eldrik @ reddit)
  (interactive)
  (save-excursion ; leave point where it is
    (goto-char (point-at-eol)) ; then go to the end of line
    (origami-toggle-node (current-buffer) (point)))) ; and try to fold
(add-hook 'origami-mode-hook
          (lambda ()
            (global-set-key (kbd "M-Z") 'custom-origami-toggle-node)
            (global-set-key (kbd "C-M-z") 'origami-toggle-all-nodes)
            (setq-local origami-fold-style 'triple-braces)))
(global-origami-mode t)

;; (La)TeX-mode
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
(defun my-latex-setup ()
  (defun update-mupdf ()
    "Updates mupdf by sending a SIGHUP signal to it."
    (interactive)
    (shell-command "killall -HUP mupdf-x11 & echo 'Update signal sent.'"))
  (define-key LaTeX-mode-map (kbd "C-c C-u") 'update-mupdf)

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

;; 6. Custom modes {{{

;; Swedish letters

;; Based on work by Moritz Ulrich <ulrich.moritz@googlemail.com>
;; Published under GNU General Public License
(define-minor-mode swedish-mode
  "A mode for conveniently using Swedish letters in Emacs.
  Note that this rebinds several important Emacs bindings,
  including ones used by xterm-mouse-mode. To use these bindings
  again, be sure to toggle the submode off."
  nil
  :lighter " åäö"
  :keymap '(("\M-[" . (lambda () (interactive) (insert ?å)))
            ("\M-'" . (lambda () (interactive) (insert ?ä)))
            ("\M-;" . (lambda () (interactive) (insert ?ö)))
            ("\M-{" . (lambda () (interactive) (insert ?Å)))
            ("\M-\"" . (lambda () (interactive) (insert ?Ä)))
            ("\M-:" . (lambda () (interactive) (insert ?Ö)))))
(provide 'swedish-mode)

;; }}}

;; 7. Customize {{{

(load custom-file)

;; }}}




