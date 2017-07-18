;; Emacs configuration
;; =============================================================================

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

;;; Package configuration

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

;;;; Various packages

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package ace-link
  :after avy
  :config
  (ace-link-setup-default))

(use-package popwin
  :init
  (require 'popwin)
  (popwin-mode 1))

(use-package helm-org-rifle
  :disabled
  :bind (("C-c f" . helm-org-rifle-current-buffer)
         ("C-c F" . helm-org-rifle)))

(use-package buffer-move
  :init
  :bind (("C-c <up>"    . buf-move-up)
         ("C-c <down>"  . buf-move-down)
         ("C-c <left>"  . buf-move-left)
         ("C-c <right>" . buf-move-right)))

(use-package aggressive-indent
  :demand
  :init
  (defun enable-aggressive-indent-mode ()
    (aggressive-indent-mode))
  (add-hook 'emacs-lisp-mode-hook       #'enable-aggressive-indent-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-aggressive-indent-mode)
  (add-hook 'ielm-mode-hook             #'enable-aggressive-indent-mode)
  (add-hook 'lisp-mode-hook             #'enable-aggressive-indent-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-aggressive-indent-mode)
  (add-hook 'scheme-mode-hook           #'enable-aggressive-indent-mode)
  (add-hook 'racket-mode-hook           #'enable-aggressive-indent-mode))

(use-package wrap-region
  :config (wrap-region-mode t))

(use-package iy-go-to-char
  :bind (("M-m" . iy-go-to-char)
         ("M-M" . iy-go-to-char-backward)
         ("C-." . iy-go-to-char-continue)
         ("C-," . iy-go-to-char-continue-backward)))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package git-gutter+
  :bind (("C-M-g C-M-g" . git-gutter+-mode))
  :bind (:map git-gutter+-mode-map
              ("C-M-g n" . git-gutter+-next-hunk)
              ("C-M-g p" . git-gutter+-previous-hunk)
              ("C-M-g d" . git-gutter+-show-hunk)
              ("C-M-g r" . git-gutter+-revert-hunks)
              ("C-M-g s" . git-gutter+-stage-hunks)
              ("C-M-g c" . git-gutter+-commit)))

(use-package openwith
  :init
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "mupdf" (file)))))

;;;; `outline-mode'

(use-package outshine
  :demand
  :ensure nil
  :load-path "packages/outshine"
  :bind (("<backtab>" . outshine-cycle-buffer)
         ("M-RET"     . outshine-insert-heading))
  :config
  (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
  (add-hook 'prog-mode-hook #'outline-minor-mode)

  ;; Narrowing now works within the headline rather than requiring to be on it:
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                     (outline-previous-visible-heading 1)))))

;; NOTE: Advice to `outshine-narrow-to-subtree' is courtesy of Eric Kaschalk
;; (http://www.modernemacs.com/post/outline-ivy/)

(use-package outline-ivy
  :ensure nil
  :load-path "packages/outline-ivy"
  :bind (:map outline-minor-mode-map
              ("s-o" . oi-jump)))

;;;; `multiple-cursors'

(use-package multiple-cursors
  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

  (require 'hydra)
  (defhydra multiple-cursors-hydra (:hint nil)
    "
      ^Up^            ^Down^        ^Other^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_/_] Edit lines
 [_P_]   Skip    [_N_]   Skip    [_?_] Mark all
 [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
 ^ ^             ^ ^             [_a_] Align
 ^ ^             ^ ^             [_q_] Quit
"
    ("/"   mc/edit-lines :exit t)
    ("?"   mc/mark-all-like-this)
    ("n"   mc/mark-next-like-this)
    ("N"   mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p"   mc/mark-previous-like-this)
    ("P"   mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r"   mc/mark-all-in-region-regexp)
    ("a"   mc/vertical-align-with-space :exit t)
    ("q"   nil))

  (with-eval-after-load 'undo-tree
    (define-key undo-tree-map (kbd "C-/") nil)
    (define-key undo-tree-map (kbd "C-?") nil))
  (global-set-key (kbd "C-/") 'multiple-cursors-hydra/body))

;;;; `visual-regexp'

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

;;;; `expand-region'

(use-package expand-region
  :config
  (require 'hydra)
  (defhydra expand-region-hydra (:hint nil)
    "
 Mark:            |  inside ~~~~~~ outside
 -----------------|-----------------------
 [_r_]   Region     |   [_'_]   Quotes   [_\"_]
 [_f_]   Function   |   [_p_]   Parens   [_P_]
 [_c_]   Comment    |   [_t_]    Tags    [_T_]
 [_c_]   Word       |   ^ ^
 -----------------|-----------------------
 [_q_]   Quit       |
   "
    ("r"  er/expand-region       :exit t)
    ("w"  er/mark-word           :exit t)
    ("'"  er/mark-inside-quotes  :exit t)
    ("\"" er/mark-outside-quotes :exit t)
    ("p"  er/mark-inside-pairs   :exit t)
    ("P"  er/mark-outside-pairs  :exit t)
    ("c"  er/mark-comment        :exit t)
    ("t"  er/mark-inner-tag      :exit t)
    ("T"  er/mark-outer-tag      :exit t)
    ("f"  er/mark-defun          :exit t)
    ("q" nil))

  :bind ("s-x" . expand-region-hydra/body))

;;;; Avy

(use-package avy
  :commands avy-isearch
  :config
  (define-key isearch-mode-map (kbd "M-g") 'avy-isearch)

  (require 'hydra)
  (defhydra avy-hydra (:hint nil)
    "
 Go to:
 -----------------------------------
 [_l_]   Line    [_L_]   Line number
 [_c_]   Char    [_s_]   Search
 [_w_]   Word    [_q_]   Quit
   "
    ("l" avy-goto-line       :exit t)
    ("L" goto-line           :exit t)
    ("s" avy-goto-char-timer :exit t)
    ("c" avy-goto-char       :exit t)
    ("w" avy-goto-word       :exit t)
    ("q" nil))

  :bind (("s-v" . avy-hydra/body)))

;;;; Paredit

(use-package paredit
  :init
  (autoload 'enable-paredit-mode
      "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)

  :config
  (defun paredit-delete-indentation (&optional arg)
    "Handle joining lines that end in a comment."
    (interactive "*P")
    (let ((comment (delete-and-extract-comment (if arg 1 0))))
      (delete-indentation arg)
      (when comment
        (save-excursion
      	  (move-end-of-line 1)
          (insert " ")
          (insert comment)))))

  (defun paredit-newline-keep-comment (&optional arg)
    "Insert newline, but keep any potential comment on the
current line."
    (interactive "*P")
    (let ((comment (delete-and-extract-comment)))
      (paredit-newline)
      (when comment
        (save-excursion
          (forward-line -1)
      	  (move-end-of-line 1)
          (insert " ")
          (insert comment)))))

  (global-set-key [remap paredit-newline] #'paredit-newline-keep-comment)

  ;; Disable comment column
  (add-hook 'paredit-mode-hook (lambda () (setq-local comment-column 0)))

  ;; Disable keybindings already used
  (define-key paredit-mode-map (kbd "M-q") nil)

  :bind (("M-R" . move-to-window-line-top-bottom)
         ("M-^" . paredit-delete-indentation)
         ("M-Q" . paredit-reindent-defun)))

;;;; Swiper, Ivy and Counsel

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  :bind (("C-s"     . swiper)

         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)

         ;; NOTE: Install `smex' to sort counsel-M-x by recently used
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c k"   . counsel-ag)

         ("C-x l"   . counsel-locate)
         ("C-x b"   . ivy-switch-buffer)
         ("C-x C-b" . ibuffer))

  :bind (:map read-expression-map
              ("C-r" . counsel-expression-history)))

(use-package counsel
  :bind
  ;; Courtesy of Pragmatic Emacs
  ;; (see http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/)
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package counsel-projectile
  :init
  (counsel-projectile-on))

;;;;; Fixing counsel-describe-*

;; The function `ivy-thing-at-point' isn't a good choice for `describe-function'
;; or `describe-variable'. Instead, I prefer the more specific functions
;; `function-called-at-point' and `variable-at-point', respectively.

;; `function-called-at-point' returns the correct function inside its /entire/
;; s-expression, whereas `ivy-thing-at-point' only returns the function at the
;; exact position of the point.

;; Ideally, `counsel.el' would be edited to use these functions instead of
;; `ivy-thing-at-point', but I'm too lazy right now to make a pull request.
;; Plus, I'm not sure everybody wants this functionality. There might also be
;; some reason that I'm not aware of, as to why `ivy-thing-at-point' is used.

(with-eval-after-load 'counsel
  (defun adv/counsel-describe-function (oldfun &optional &rest r)
    (interactive)
    (cl-letf (((symbol-function 'ivy-thing-at-point)
               (lambda ()
                 (-> (function-called-at-point)
                     (symbol-name)))))
      (call-interactively oldfun)))
  (advice-add #'counsel-describe-function :around #'adv/counsel-describe-function)

  (defun adv/counsel-describe-variable (oldfun &optional &rest r)
    (interactive)
    (cl-letf (((symbol-function 'ivy-thing-at-point)
               (lambda ()
                 (--> (variable-at-point)
                      (if (eq it 0) (intern "") it)
                      (symbol-name it)))))
      (call-interactively oldfun)))
  (advice-add #'counsel-describe-variable :around #'adv/counsel-describe-variable))

;;;;; counsel-everything

(with-eval-after-load 'ivy
  ;; TODO: Add support for ignoring absolute paths

  (defun counsel-everything (&optional dir)
    "Find all files in the current directory, including subdirectories.
If DIR is non-nil, use that directory instead of current one.

From the minibuffer, C-<backspace> can be pressed to go up a
directory, relative to DIR. Note that by default, you cannot go
up to /, as processing this directory takes a long time and is
rarely desirable."
    (interactive)
    (let* ((dir (or dir "."))
           (abs-path (file-truename dir))
           (dir-name (file-name-nondirectory (directory-file-name abs-path)))
           (ignored-dirs '("*/.git"
                           "*/elpa"
                           "*/state"))
           (ignored-dirs-string (mapconcat
                                 (lambda (dir)
                                   (concat "-not \\( -path '" dir "' -prune \\)"))
                                 ignored-dirs " "))
           (candidates (mapcar
                        (lambda (file)
                          (if (equal dir file)
                              nil
                            (if (string-prefix-p "find: " file)
                                ;; Ignore errors from `find' (usually about
                                ;; permissions):
                                nil
                              (string-remove-prefix (concat dir "/") file))))
                        (split-string
                         (shell-command-to-string (concat "find " dir " "
                                                          ignored-dirs-string))
                         "\n" t))))
      (setf counsel-everything--dir dir) ; set global dir value (for counsel-everything-up-directory)
      (ivy-read (format "(%s) Find everything: " dir-name) candidates
                :matcher #'counsel--find-file-matcher
                :action (lambda (file)
                          (with-ivy-window
                            (find-file (expand-file-name file counsel-everything--dir))))
                :keymap (make-counsel-everything-map)
                :require-match 'confirm-after-completion
                :caller #'counsel-everything)))

  (ivy-set-actions
   #'counsel-everything
   '(("j" find-file-other-window "other window")
     ("x" counsel-find-file-extern "open externally")
     ("r" counsel-find-file-as-root "open as root")))

  (defvar counsel-everything--dir nil
    "Internal variable used by counsel-everything-up-directory.")

  (defun make-counsel-everything-map ()
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-DEL")         #'counsel-everything-up-directory)
      (define-key map (kbd "C-<backspace>") #'counsel-everything-up-directory)
      map))

  (defun counsel-everything-up-directory ()
    (interactive)
    (let ((dir "")
          (up-dir))
      (if (and (boundp 'counsel-everything--dir)
               (not (equal counsel-everything--dir ".")))
          (setf dir (concat counsel-everything--dir "/")))
      (setf up-dir (format "%s.." dir))
      (unless (string-equal (file-truename up-dir) "/")
        (eval `(run-at-time nil nil
                            (lambda ()
                              (counsel-everything ,up-dir))))
        (minibuffer-keyboard-quit))))

  (global-set-key (kbd "C-x C-M-f") #'counsel-everything))

;;;; Projectile

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)

  (setq projectile-globally-ignored-directories
        (cl-list* ".cache" ".cargo"
                  projectile-globally-ignored-directories)))

;;;; Magit

(use-package magit
  :bind ("C-c g" . magit-status))

;;;; Org

(use-package org
  :mode (("\\.org$" . org-mode))
  :demand
  :config
  ;; Make ' and " work in inline code
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components) ; reload setting

  ;; Make windmove work in org-mode (not very useful)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Export to exports/ subdirectory
  (defun /org-export-to-subdirectory (orig-fun &rest args)
    (shell-command (concat "mkdir -p exports"))
    (apply orig-fun
           (pop args)                     ; backend
           (concat "exports/" (pop args)) ; file
           args))
  (advice-add #'org-export-to-file :around #'/org-export-to-subdirectory)

  ;; Remove keybindings that I already use
  (define-key org-mode-map (kbd "C-'") nil)

  ;; Enable for all Org files
  (add-hook 'org-mode-hook #'swedish-mode) ; Swedish letters
  (add-hook 'org-mode-hook #'org-autolist-mode) ; better list behavior

  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-iswitchb)))

;;;;; org-tree-view

(with-eval-after-load 'org

  (defvar org-tree-view-mode-map (make-sparse-keymap))

  (define-key org-tree-view-mode-map (kbd "<return>")  #'org-tree-view/open-headline-at-point)
  (define-key org-tree-view-mode-map (kbd "<mouse-1>") #'org-tree-view/open-headline-at-point)
  (define-key org-tree-view-mode-map (kbd "C-g")       #'org-tree-view/close)
  (define-key org-tree-view-mode-map (kbd "<tab>")     #'org-tree-view/toggle-visibility)
  (define-key org-tree-view-mode-map [remap end-of-buffer]
    (lambda () (interactive) (goto-char (point-max)) (move-beginning-of-line 1)))

  ;; Re-define self-insert-command
  (map-keymap (lambda (key fun)
                (if (equal fun #'self-insert-command)
                    ;; Append ((from . to) #'org-tree-view/self-insert-command))
                    (nconc org-tree-view-mode-map
                           (list (cons key #'org-tree-view/self-insert-command)))))
              (current-global-map))
  (setq org-tree-view-mode-map ; develop (from . to) ranges, please
        (keymap-canonicalize org-tree-view-mode-map))

  (define-minor-mode org-tree-view-mode
      ""
    nil
    :lighter " tree-view"
    org-tree-view-mode-map)

  (provide 'org-tree-view-mode)
  (require 'subr-x)

  (defun org-tree-view/get-headlines (&optional level &optional base-buffer)
    "Get a list of all headlines in `base-buffer' of a level less
  than or equal to provided `level'. Includes headlines outside of
  any potential narrowing."
    (let ((level (or level org-tree-view/level))
          (base-buffer
           (or base-buffer
               (if (org-tree-view/is-tree-view)
                   (get-buffer (org-tree-view/make-base-buffer-name))
                 (current-buffer))))
          (headlines))
      (with-current-buffer base-buffer
        (let ((widened-buffer
               (clone-indirect-buffer
                (concat "<widened>" (buffer-name base-buffer)) nil)))
          (with-current-buffer widened-buffer
            (widen)
            (setq headlines (org-element-map (org-element-parse-buffer 'headline) 'headline
                              (lambda (headline)
                                (when (<= (org-element-property :level headline) level)
                                  headline)))))
          (kill-buffer widened-buffer)))
      headlines))

  (defun org-tree-view/draw-headline (headline)
    "Return a string of the headline to be printed, with the proper
  face and its position in the base buffer encoded as the
  `org-tree-view-headline-pos' text property."
    (let* ((title (org-element-property :raw-value headline))
           (level (org-element-property :level headline))
           (begin (org-element-property :begin headline))
           (end (org-element-property :end headline))
           (org-tree-view-level-face (intern
                                      (concat "org-tree-view/level-"
                                              (number-to-string level))))
           (text (concat (apply #'concat (make-list (* 2 (1- level)) " "))
                         "* "
                         (replace-regexp-in-string "\"" "" title nil t)))) ; remove \" from title

      ;; Text properties
      (put-text-property ; org-tree-view-level-N
       0 (length text)
       'font-lock-face org-tree-view-level-face
       text)
      (put-text-property ; encode headling position in base buffer
       0 (length text)
       'org-tree-view-headline-pos begin
       text)
      (setq text (org-tree-view/draw-string text))

      ;; Pad headline with spaces
      (let ((end (1- (length text))))
        (setq text (concat text
                           (apply #'concat (make-list (* 2 org-tree-view/width) " "))))
        ;; Add same properties to padding (a little repetitive)
        (put-text-property
         end (length text)
         'font-lock-face org-tree-view-level-face
         text)
        (put-text-property
         end (length text)
         'org-tree-view-headline-pos begin
         text))
      text))

  (defun org-tree-view/draw-string (text)
    "Apply the appropriate faces on `text' according to Org markup
  syntax and return the resulting string."
    (let* ((types '(link
                    italic
                    bold
                    strike-through
                    verbatim
                    code))
           (data (org-element-parse-secondary-string text types)))
      (org-element-map data types
        (lambda (object)
          (let* ((type (org-element-type object))
                 (begin (1- (org-element-property :begin object)))
                 (end (1- (org-element-property :end object)))
                 (faces
                  '(italic         italic
                    bold           bold
                    strike-through (:strike-through t)
                    verbatim       org-verbatim
                    code           org-code)))

            ;; Figure out real end of object
            (let ((substr (string-trim-right (substring text begin end))))
              (setq end (+ begin (length substr))))

            ;; Handle faces
            (when (member type faces)
              (let* ((face (plist-get faces type))
                     (existing-face (get-text-property begin 'font-lock-face text))
                     (new-face (if existing-face
                                   `(,face ,existing-face)
                                 face)))
                (put-text-property ; add face for type
                 begin end
                 'font-lock-face new-face
                 text))
              (put-text-property ; remove first piece of markup
               begin (1+ begin)
               'display ""
               text)
              (put-text-property ; remove second piece of markup
               (1- end) end
               'display ""
               text))

            ;; Handle links
            (when (equal 'link type)
              (let ((contents-begin (org-element-property :contents-begin object)))
                (if contents-begin ; has contents
                    (put-text-property
                     begin (1- contents-begin)
                     'display ""
                     text)
                  (put-text-property ; only url
                   begin (+ begin 2)
                   'display ""
                   text))
                (put-text-property
                 (- end 2) end
                 'display ""
                 text))))))
      text))

  (defun org-tree-view/insert-headlines ()
    "Insert all drawn headlines at the current position."
    (let ((headlines (org-tree-view/get-headlines)))
      (cl-loop for headline in headlines
         do (insert (concat
                     (org-tree-view/draw-headline headline)
                     "\n")))
      ;; Delete final newline
      (backward-delete-char 1)))

  (defun org-tree-view/refresh (&optional no-set-window-start)
    (let* ((orig-window-start (window-start))
           (orig-window-line  (+ (count-lines (window-start) (point))
                                 (if (= (current-column) 0) 1 0)
                                 -1))
           ;; ^ see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_478.html
           (refresh `(progn
                       (setq-local buffer-read-only nil)
                       (set-window-fringes (get-buffer-window) 8 1)
                       (erase-buffer)
                       (org-tree-view/insert-headlines)
                       (set-window-start (get-buffer-window) orig-window-start)
                       (goto-char (window-start))
                       (forward-line orig-window-line)
                       (move-beginning-of-line 1)
                       (setq-local buffer-read-only t))))
      (if (org-tree-view/is-tree-view)
          (eval (macroexpand refresh))
        (if (org-tree-view/has-tree-view)
            (with-current-buffer (org-tree-view/make-tree-view-buffer-name)
              (eval (macroexpand refresh)))))))
  (defun org-tree-view/open (&optional arg)
    ""
    (interactive "p")
    (let ((tree-view-buffer-name (org-tree-view/make-tree-view-buffer-name))
          (base-buffer (current-buffer))
          (tree-view-buffer))

      (if (and (org-tree-view/has-tree-view)
               (get-buffer-window tree-view-buffer-name))
          (progn
            (select-window (get-buffer-window tree-view-buffer-name))
            (setq tree-view-buffer (get-buffer tree-view-buffer-name)))
        (if (org-tree-view/has-tree-view)
            (kill-buffer (org-tree-view/make-tree-view-buffer-name)))
        (if (equal org-tree-view/side 'left)
            (split-window-right org-tree-view/width)
          (split-window-right (* -1 org-tree-view/width))
          (other-window 1))
        (setq tree-view-buffer (generate-new-buffer tree-view-buffer-name)))

      ;; Switch to tree view buffer
      (switch-to-buffer tree-view-buffer)
      (org-tree-view/setup)
      (org-tree-view-mode)))

  (defun org-tree-view/close (&optional tree-view-buffer &optional base-buffer)
    "Close `tree-view-buffer' for `base-buffer'. Defaults to the current buffer."
    (interactive)
    (let* ((base-buffer
            (get-buffer (or base-buffer
                            (if (org-tree-view/is-tree-view)
                                (org-tree-view/make-base-buffer-name)
                              (current-buffer)))))
           (tree-view-buffer
            (get-buffer (or tree-view-buffer
                            (org-tree-view/make-tree-view-buffer-name base-buffer))))
           (tree-view-window))
      (if tree-view-buffer
          (progn (if (setq tree-view-window (get-buffer-window tree-view-buffer))
                     (delete-window tree-view-window))
                 (kill-buffer tree-view-buffer)
                 (org-tree-view/cleanup))
        (error "No tree view found!"))))

  (defun org-tree-view/bind-close-after (&optional key)
    "Bind the key combination pressed to call
  `org-tree-view/open-headline-at-point' to also close the tree
  view, but only for 1 second."
    (let* ((key (or key (this-command-keys-vector)))
           (original-binding (local-key-binding key))
           (reset-key `(local-set-key ,key (quote ,original-binding)))
           (base-buffer))
      (if (org-tree-view/is-tree-view)
          (setq base-buffer (org-tree-view/make-base-buffer-name))
        (if (org-tree-view/has-tree-view)
            (setq base-buffer (current-buffer))))
      (when (and base-buffer (not (string-match-p "mouse" (key-description key))))
        (message "Press %s again to close the tree view." (key-description key))
        (eval
         (macroexpand
          `(progn
             (local-set-key ,key (lambda () (interactive)
                                         ,reset-key
                                         (org-tree-view/close)))
             (run-with-timer 1 nil (lambda ()
                                     (message " ") ; clear
                                     (with-current-buffer ,base-buffer
                                       ,reset-key)))))))))

  (defun org-tree-view/open-headline-at-point (&optional bind-close-after)
    "From tree view, open headline at point in base buffer. If
  `bind-close-after' is non-nil (default: t), the function will
  also call `org-tree-view/bind-close-after' before finishing."
    (interactive)
    (let* ((bind-close-after (or bind-close-after t))
           (base-buffer (org-tree-view/make-base-buffer-name))
           (position (get-text-property (point) 'org-tree-view-headline-pos)))
      (condition-case nil
          (select-window (car (get-buffer-window-list base-buffer)))
        (error (other-window 1)
               (switch-to-buffer base-buffer)))
      (if position
          (progn (widen)
                 (goto-char position)
                 (outline-show-all)
                 (org-cycle-hide-drawers 'all)
                 (org-narrow-to-subtree)
                 (if bind-close-after (org-tree-view/bind-close-after)))
        (error "No headline found!"))))

  (defun org-tree-view/switch-to-base-buffer ()
    (interactive)
    (when (org-tree-view/is-tree-view)
      (if (get-buffer-window (org-tree-view/make-base-buffer-name))
          (select-window (get-buffer-window (org-tree-view/make-base-buffer-name)))
        (other-window 1)
        ;; FIXME: the following switch-to-buffer didn't work last I checked
        (switch-to-buffer (org-tree-view/make-base-buffer-name) nil :force-same-window))))

  (defun org-tree-view/toggle-visibility (&optional arg)
    (interactive "P")
    (if (numberp arg)
        (setq-local org-tree-view/level arg)
      (if (/= org-tree-view/level (default-value 'org-tree-view/level))
          (setq-local org-tree-view/level (default-value 'org-tree-view/level))
        (setq-local org-tree-view/level 2)))
    (org-tree-view/refresh :no-set-window-start))
  (defun org-tree-view/self-insert-command (N)
    "Start an `isearch' with the character `N' as the first
  character in the search."
    (interactive "p")
    (goto-char (point-min))
    (let* ((char (string-to-char (this-command-keys)))
           (unread-command-events (append unread-command-events (list char))))
      (isearch-forward)))

  (defun org-tree-view/isearch-return ()
    "Open the matching headline and exit the isearch."
    (interactive)
    (when (org-tree-view/is-tree-view)
      (org-tree-view/open-headline-at-point)
      (run-with-timer 0 nil (lambda () (org-tree-view/bind-close-after (kbd "<S-return>"))))
      ;; ^ Timer needed because isearch-exit behaves weirdly otherwise.
      (let ((inhibit-message t))
        (isearch-exit))))
  (defun org-tree-view/setup ()
    (font-lock-mode)
    (org-tree-view/refresh)
    (setq-local buffer-read-only t)
    (setq-local scroll-margin 0)
    ;; If evil-mode is enabled, enable emacs state:
    (if (and (boundp 'evil-mode) evil-mode) (evil-emacs-state))
    (setq-local case-fold-search t) ; ignore case
    (define-key isearch-mode-map (kbd "<S-return>") #'org-tree-view/isearch-return))

  (defun org-tree-view/cleanup ()
    (define-key isearch-mode-map (kbd "<S-return>") nil))

  (defun org-tree-view/make-base-buffer-name (&optional tree-view-buffer)
    (let ((tree-view-buffer
           (get-buffer (or tree-view-buffer
                           (current-buffer)))))
      (string-remove-prefix "<tree>" (buffer-name tree-view-buffer))))

  (defun org-tree-view/make-tree-view-buffer-name (&optional base-buffer)
    (let ((base-buffer
           (get-buffer (or base-buffer
                           (current-buffer)))))
      (concat "<tree>" (buffer-name base-buffer))))

  (defun org-tree-view/is-tree-view (&optional buffer)
    (let ((buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
        (bufferp (get-buffer (org-tree-view/make-base-buffer-name buffer))))))

  (defun org-tree-view/has-tree-view (&optional buffer)
    (let ((buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
        (bufferp (get-buffer (org-tree-view/make-tree-view-buffer-name buffer))))))
  (defgroup org-tree-view nil
    "A window providing easy access to all headings in an Org document."
    :group 'org-mode)

  (defcustom org-tree-view/level 3
    "The highest level headings to show in the tree view. Think of
  each asterisk preceding a heading as a level."
    :group 'org-tree-view
    :type 'integer)

  (defcustom org-tree-view/width 30
    "The width of the tree view window."
    :group 'org-tree-view
    :type 'integer)

  (defcustom org-tree-view/side 'left
    "The side on which to open the tree view window."
    :group 'org-tree-view
    :type '(radio
            (const :tag "Left" left)
            (const :tag "Right" right)))

  (defcustom org-tree-view/open-key-binding "C-c C-t"
    "The key binding to open the tree view."
    :group 'org-tree-view
    :type 'string
    :set (lambda (option value)
           (define-key org-mode-map (kbd value)
             #'org-tree-view/open)
           (define-key org-tree-view-mode-map (kbd value)
             #'org-tree-view/switch-to-base-buffer)))

  ;; Define org-tree-view/level-N faces
  (dotimes (i 8)
    (let* ((num (number-to-string (1+ i)))
           (org-tree-view-face (intern (concat "org-tree-view/level-" num)))
           (org-face (intern (concat "org-level-" num))))
      (eval (macroexpand
             `(defface ,org-tree-view-face
                  '((t . (:inherit ,org-face)))
                (format "Face for level %s headlines." num))))))

;;;;; Automatic wiktionary links

  (defun org-make-wiktionary-link (string &optional from to)
    "Wraps the word at point or selected word in a Wiktionary link to the word."

    ;; (see http://ergoemacs.org/emacs/elisp_command_working_on_string_or_region.html)
    (interactive
     (if (use-region-p)
         (list nil (region-beginning) (region-end))
       (let ((bds (bounds-of-thing-at-point 'word)) )
         (list nil (car bds) (cdr bds)))))

    (let ((wiktionary-language
           (if (boundp 'wiktionary-language) wiktionary-language 'russian)))

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
        (insert output))))

  (define-key org-mode-map (kbd "C-c L") #'org-make-wiktionary-link)

;;;;; Tangling my configuration

  (defun org-babel-tangle-config ()
    (interactive)

    (let ((tangle-buffer (clone-indirect-buffer "<tangle>config" nil :norecord)))
      (with-current-buffer tangle-buffer
        (widen)
        (goto-char (org-babel-find-named-block "c:config-all"))
        (let ((current-prefix-arg '(4)))
          (call-interactively #'org-babel-tangle)))
      (kill-buffer tangle-buffer))
    (byte-compile-file "config.el"))

  (define-key org-mode-map (kbd "C-c C-v M-t") #'org-babel-tangle-config)

;;;;; Fixing `org-beginning-of-line'

  (defun org-smarter-beginning-of-line (original-function &optional n)
    "The exact same function as `org-beginning-of-line',
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

  (advice-add 'org-beginning-of-line :around #'org-smarter-beginning-of-line))

;;;; Racket

(use-package racket-mode
  :bind (:map racket-repl-mode-map
              ("<f5>" . /racket-repl-run))
  :bind (:map racket-mode-map
              ("<S-f5>" . /racket-interrupt-run))
  :config
  (defun /racket-repl-run ()
    "From any buffer (usually the Racket REPL buffer), run the
    Racket program in the first buffer found whose name ends with
    \".rkt\"."
    (interactive)
    (let ((rkt-buffer (car (cl-remove-if-not
                            (lambda (buffer)
                              (string-suffix-p ".rkt" (buffer-name buffer)))
                            (buffer-list))))) ; assume first .rkt buffer
      (with-current-buffer rkt-buffer
        (racket-run))))
  (defun /racket-interrupt-run ()
    "Run the Racket program in the current buffer, after sending
    an interrupt signal to the Racket REPL (C-c)."
    (interactive)
    (with-current-buffer racket--repl-buffer-name
      (comint-interrupt-subjob))
    (racket-run))
  (add-hook 'racket-repl-mode-hook #'toggle-truncate-lines))

;;;; LaTeX

(use-package tex
  :defer t
  :ensure auctex
  :config
  (require 'auctex-latexmk)
  (auctex-latexmk-setup)

  (defun /start-update-viewer ()
    "Starts/updates PDF viewer."
    (interactive)
    (if (string-match "no process found"
                      (shell-command-to-string "killall -HUP mupdf-x11"))
        (error "PDF viewer is not running")))

  (defun /run-view ()
    "Saves the current LaTeX document, processes it and finally runs it."
    (interactive)
    (save-buffer)
    (let ((process (TeX-run-TeX "LaTeX"
                                (format "latexmk -xelatex %s" (buffer-file-name))
                                (file-name-base (buffer-file-name)))))
      (set-process-sentinel process (lambda (process sentinel)
                                      (when (= 0 (process-exit-status process))
                                        (/start-update-viewer))))))

  (defun /view-after-run (&rest r)
    (/start-update-viewer))

  (advice-add #'Latexmk-sentinel :after #'/view-after-run)
  (add-hook 'TeX-mode-hook (lambda () (setf TeX-command-default "LatexMk")))

  :bind (:map LaTeX-mode-map
              ("C-c C-u" . /start-update-viewer)))

;;; Basic preferences

(server-start)         ; use emacs as a server
(electric-pair-mode 1) ; auto-insert matching pairs
(menu-bar-mode -1)     ; disable menu bar
(tool-bar-mode -1)     ; disable gui toolbar
(save-place-mode 1)    ; save cursor position
(xterm-mouse-mode 1)   ; use mouse (somewhat effectively) in terminal

;; Activate auto-fill-mode for all text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Always show trailing whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; Shebang mode detection
(add-to-list 'interpreter-mode-alist
             '("python3" . python-mode))

;;;; Python `shell-compile'
;; TODO: Move into `use-package' declaration
(defun shell-compile () ; (courtesy of djangoliv @ stack interchange)
  (interactive)
  (shell-command (concat "python " (buffer-file-name)))
  (if (<= (* 2 (window-height)) (frame-height))
      (enlarge-window 20)
    (/ (frame-height) 2)))
(add-hook 'python-mode-hook
          '(lambda ()
            (define-key python-mode-map (kbd "C-c C-c") 'shell-compile)))

;;;; Silent mouse-wheel scrolling

(defun silent-mwheel-scroll (oldfun &rest r)
  (interactive (list last-input-event))
  (ignore-errors
    (call-interactively oldfun)))

(advice-add #'mwheel-scroll :around #'silent-mwheel-scroll)

;;;; Directories

(setf custom-file (concat user-emacs-directory "custom.el"))

(setf emacs-state-directory (expand-file-name "state/" user-emacs-directory))
(defun state-dir (file)
  (concat emacs-state-directory file))

;;;;; Various places

(setf auto-save-list-file-prefix   (state-dir "auto-save-list/.saves~")
      save-place-file              (state-dir "save-place")
      recentf-save-file            (state-dir "recentf")
      ido-save-directory-list-file (state-dir "ido.last")
      eshell-directory-name        (state-dir "eshell")
      nsm-settings-file            (state-dir "network-security.data")
      backup-directory-alist       `((".*" . ,(state-dir "saves"))))

;;;;;; Tramp

(setf tramp-backup-directory-alist backup-directory-alist)
(setf tramp-auto-save-directory (state-dir
                                        "tramp/auto-save/"))
(setf tramp-persistency-file-name (state-dir
                                   "tramp/persistency.el"))

;;;;;; Projectile
(setf projectile-cache-file (concat emacs-state-directory
                                    "projectile/cache.el"))
(setf projectile-known-projects-file
      (concat emacs-state-directory
              "projectile/known-projects.el"))

;;;;;; File for activated disabled commands

(defadvice en/disable-command (around put-in-custom-file activate)
  "Put declarations in `custom-file'."
  (let ((user-init-file (concat user-emacs-directory ".commands")))
    ad-do-it))
(load-file (concat user-emacs-directory ".commands"))
(defun delete-and-extract-comment (&optional bol-arg)
  "Delete and return the comment at the end of the line. If there
is no comment, return nil."
  (let (comment)
    (save-excursion
      (move-beginning-of-line (or bol-arg 1))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comment (delete-and-extract-region (point) (point-at-eol)))
        (delete-horizontal-space)))
    (if (string-equal "" comment) nil comment)))

;;;; Lisp indentation

(setf lisp-indent-function 'common-lisp-indent-function)

;; Fix custom indentation
(with-eval-after-load "cl-indent"
  (put 'use-package 'common-lisp-indent-function 1)
  (put 'use-package-as-one 'common-lisp-indent-function 1)
  (put 'defface 'common-lisp-indent-function 1))

;;;; Functions

(defmacro --fcase (form &rest arg-then)
  "For each `arg-then', evaluate `form' with the car of
`arg-then' exposed as `it'. For the first `arg-then' for which
the evaluation of `form' returns true, evaluate and return then
cdr of `arg-then'.

If the car of `arg-then' is `else', its cdr will be always be
evaluated, if no `arg-then' before has resulted in a true
evaluation of `form'.

   (--fcase (string-prefix-p it my-string)
            (\"prefix-\"       (message \"Prefix: prefix-\"))
            (\"other-prefix-\" (message \"Prefix: other-prefix-\"))
            (else              (message \"No prefix\")))"
  (declare (debug (form form arg-then form)))
  `(cl-loop
      for it-then in ',arg-then
      do (let ((it   (car it-then))
               (then (cdr it-then)))
           (unless (eq it 'else) (setf it (eval it)))
           (if (or (eq it 'else) ,form)
               (cl-return
                 (car (last (mapcar
                             (lambda (exp) (eval exp))
                             then))))))))

;;;;; Session management (`desktop-save-mode')

(setq desktop-dirname             (concat emacs-state-directory "desktop/")
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t)

(defun init-desktop (&optional arg)
  "Load the desktop (if C-u is provided) and enable autosaving."
  (interactive "p")
  (if current-prefix-arg (desktop-read))
  (desktop-save-mode 1)
  (message "Desktop-Save mode enabled"))

(global-set-key (kbd "C-c D") 'init-desktop)

;;;;; Alignment

(defun align-comments-in-region (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))

(global-set-key (kbd "C-c M-a") #'align-comments-in-region)

;;;;; `smart-open-line'

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
  (newline)
  (if (looking-at "[[:space:]]*$") ; remove indentation from old line
      (delete-horizontal-space))
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)

;;;;; `smarter-move-{beginning|end}-of-line'

(defun smarter-move-beginning-of-line (&optional &rest args)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line. If ARG is not nil or 1, move forward
ARG - 1 lines first. If point reaches the beginning or end of the
buffer, stop there."
  (interactive "^p")
  (let ((arg (or (prefix-numeric-value current-prefix-arg) 1)))
    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))) ; based on function from Emacs Redux

(defun smarter-move-end-of-line (&optional &rest args)
  "Move to the end of the line, but before any potential comment.
If already at the pre-comment end of line, move to the actual end
of line. If ARG is not nil or 1, move forward ARG - 1 lines
first. If point reaches the beginning or end of the buffer, stop
there."
  (interactive "^p")
  (let ((arg (or (prefix-numeric-value current-prefix-arg) 1)))
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (let ((bol-point (point)))
        (let ((comment-start (comment-search-forward (point-at-eol) t)))
          (if (and comment-start
                   (not (eq comment-start bol-point)))
              (progn (goto-char comment-start)
                     (skip-syntax-backward " " (point-at-bol)))
            (move-end-of-line 1)))
        (when (= orig-point (point))
          (move-end-of-line 1))))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line]
                'smarter-move-end-of-line)

;;;; Modes
;;;;; xmodmap-mode

;; NOTE: Courtesy of the EmacsWiki
;; (See https://www.emacswiki.org/emacs/XModMapMode)

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")
(add-to-list 'auto-mode-alist '("\\.Xmodmap\\'" . xmodmap-mode))

;;; Keybindings

(global-set-key (kbd "C-x 4 e") ; open eshell in split to the right
                (lambda ()
                  (interactive)
                  (split-window-right)
                  (other-window 1)
                  (eshell)))

(global-set-key (kbd "M-<f1>") #'menu-bar-mode)

(global-set-key (kbd "M-]") #'other-window)
(global-set-key (kbd "M-[") (lambda (n)
                              (interactive "p")
                              (other-window (* -1 n))))

(global-set-key (kbd "<S-home>") #'next-buffer)
(global-set-key (kbd "<S-end>") #'previous-buffer)

(global-set-key (kbd "C-h C-t") #'toggle-debug-on-error)

(global-set-key (kbd "M-n") (lambda (n) (interactive "p") (scroll-up n)))
(global-set-key (kbd "M-p") (lambda (n) (interactive "p") (scroll-down n)))
;; Use for other modes too
(define-key Info-mode-map    (kbd "M-n") (lookup-key global-map (kbd "M-n")))
(define-key Info-mode-map    (kbd "M-p") (lookup-key global-map (kbd "M-p")))

(add-hook 'message-mode-hook
          (lambda ()
            (define-key message-mode-map (kbd "M-n") (lookup-key global-map (kbd "M-n")))))

(global-set-key (kbd "<C-tab>") #'completion-at-point)

;;;; Disable C-z for GUI Emacs

(defun suspend-frame-unless-gui (oldfun &rest r)
  (unless (display-graphic-p) (apply oldfun r)))

(advice-add #'suspend-frame :around #'suspend-frame-unless-gui)

;;; Themes

(defvar current-theme nil
  "Currently selected theme.")

;;;; Daylight-dependent default theme

(setf light-theme 'leuven
      dark-theme  'gruvbox-dark-soft)

(defun /daylight-sets-color ()
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

(add-hook 'after-init-hook #'/daylight-sets-color)

;;;; Dynamic theme configuration

(advice-add 'load-theme :after (lambda (theme &optional no-confirm no-enable)
                                 (unless no-enable (theme-do-all theme))))

;;;;; `theme-do-all'

(defun theme-do-all (theme)
  "Actions to perform whenever a theme is loaded."

  (setf current-theme theme)

  (require 'color) ; for color-* functions

  (let* ((bg
          (alist-get 'background-mode (frame-parameters)))
         (intensify
          (if (eq bg 'dark) 'color-darken-name 'color-lighten-name))
         (anti-intensify
          (if (eq bg 'dark) 'color-lighten-name 'color-darken-name)))

    ;; Settings for all themes
    ;; ***********************

    ;; * Dynamic mouse pointer color
    (set-mouse-color
     (if (eq bg 'dark) "#ffffff" "#000000"))

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

    ;; * org-block-begin-line, org-block-end-line
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
           :slant (face-attribute 'default :slant)))

    ;; * ivy-current-match -> remove underline and add background
    (when (eq (face-attribute 'ivy-current-match :underline) t)
      (set-face-attribute 'ivy-current-match nil :underline 'unspecified)
      (when (eq (face-attribute 'ivy-current-match :background) 'unspecified)
        (let* ((default-bg (face-attribute 'default :background))
               (bg-1       (funcall intensify default-bg 15))
               (bg-2       (funcall anti-intensify default-bg 15))
               ;; Ensure background isn't too dark or light:
               (final-bg   (if (if (eq bg 'dark)
                                   (> (color-distance bg-1 "#000000") 5000)
                                 (> (color-distance bg-1 "#FFFFFF") 5000))
                               bg-1
                             bg-2)))
          (set-face-attribute 'ivy-current-match nil :background final-bg))))

    ;; * fix italic faces
    ;; (mapc
    ;;  (lambda (face)
    ;;    (when (eq (face-attribute face :slant) 'italic)
    ;;      (set-face-attribute face nil :family "Fira Mono Medium")))
    ;;  (face-list))

    ;; * Reset fci-mode
    (let ((inhibit-message t))
      (call-interactively 'fci-mode)
      (call-interactively 'fci-mode))

    ;; Settings for specific themes
    ;; ****************************

    (cl-case theme
      ;; * Gruxbox
      ('gruvbox
       (custom-theme-set-faces
        'gruvbox ; fix hard-to-see org-mode colors
        '(org-verbatim ((t (:foreground "DarkGray"))))
        '(org-document-info-keyword ((t (:foreground "DarkGoldenrod"))))))

      ;; * Tango
      ('tango
       (custom-theme-set-faces
        'tango
        '(hl-line ((t (:background "#dddddd"))))))

      ;; * Tango-dark
      ('tango-dark
       (custom-theme-set-faces
        'tango-dark ; fix crazy hl-line (bright yellow per default!)
        '(hl-line ((t (:background "#444444")))))))

    ;; * Eziam
    (when (or (equal theme 'eziam-dark) (equal theme 'eziam-light))
      (custom-theme-set-faces
       theme
       ;; - Less contrastive cursor
       `(cursor ((t (:background
                     ,(funcall
                       anti-intensify
                       (face-attribute 'default :foreground) 25)))))
       ;; - More contrastive paren match
       `(show-paren-match ((t (:background
                               ,(funcall
                                 anti-intensify
                                 (face-attribute 'default :background) 25))))))
      ;; - Fading rainbow-delimiters (from black to light gray)
      (cl-loop for n in (number-sequence 1 9)
         do (let ((face-name (concat
                              "rainbow-delimiters-depth-"
                              (number-to-string n)
                              "-face"))
                  (color (funcall
                          intensify
                          (face-attribute 'default :foreground)
                          (* n 6))))
              (custom-theme-set-faces
               theme
               (list (intern face-name) `((t (:foreground ,color))))))))))

;;;;; `theme-reload'

(defun theme-reload ()
  (interactive)
  (if (boundp 'current-theme)
      (theme-do-all current-theme)))

(global-set-key (kbd "C-c R") #'theme-reload)

;;;; Disable previous theme when enabling new theme

(add-hook 'after-init-hook
          (lambda () (defadvice load-theme
                         (before theme-dont-propagate activate)
                       (mapc #'disable-theme custom-enabled-themes))))

;;; E-mail
;;;; Gnus

;; For complete configuration, see ~/.config/emacs/gnus/gnus.el.

(defun gnus-dir (&optional path) (concat user-emacs-directory "gnus/" path))
(setf gnus-init-file (gnus-dir "gnus.el"))

;;;; notmuch

(defvar notmuch-hello-refresh-count 0)

(defun notmuch-hello-refresh-status-message ()
  (let* ((new-count
          (string-to-number
           (car (process-lines notmuch-command "count"))))
         (diff-count (- new-count notmuch-hello-refresh-count)))
    (cond
      ((= notmuch-hello-refresh-count 0)
       (message "You have %s messages."
                (notmuch-hello-nice-number new-count)))
      ((> diff-count 0)
       (message "You have %s more messages since last refresh."
                (notmuch-hello-nice-number diff-count)))
      ((< diff-count 0)
       (message "You have %s fewer messages since last refresh."
                (notmuch-hello-nice-number (- diff-count)))))
    (setq notmuch-hello-refresh-count new-count)))

(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)
(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

;;; Language configuration
;;;; `ispell'

;; Ignore the following Org markup:

(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC ". "#\\+END_SRC$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE ". "#\\+END_EXAMPLE$"))
(add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
(add-to-list 'ispell-skip-region-alist '("\\[fn:.+:" . "\\]"))
(add-to-list 'ispell-skip-region-alist '("=.*" . ".*=")) ; org verbatim

;;;; `swedish-mode'

;; Based on `umlaut-mode'by Moritz Ulrich <ulrich.moritz@googlemail.com>
;; (See https://gist.github.com/the-kenny/394120)
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

;;;; Combining acute accent

;; Very useful for marking stress when doing Russian homework.

(global-set-key (kbd "C-c 8 '") (lambda () (interactive) (insert-char 769)))

;;; Custom hooks
;;;; `window-focus-out-hook', `window-focus-in-hook'

(defun run-window-focus-out-hook (window &optional norecord)
  (run-hooks 'window-focus-out-hook))
(defun run-window-focus-in-hook (window &optional norecord)
  (run-hooks 'window-focus-in-hook))

(advice-add 'select-window :before 'run-window-focus-out-hook)
(advice-add 'select-window :after 'run-window-focus-in-hook)


;; TODO: Add exception for magit buffer switching.
;; NOTE: This doesn't always play nice with magit. For example, select-window
;; seems to be run when opening the commit message buffer, but *not* when
;; returning to the magit status buffer. I'm not quite sure why, but I suppose I
;; could add an exception for it. I'd have to look at the magit source. Perhaps
;; I could just run a function upon switch-to-buffer that checks whether the
;; current-window is different from the previous-current-window (saved in a
;; variable); that might be the most simple solution, similar to what hl-line
;; does, but as I've said before, more efficient than attaching everything to
;; post-command-hook ...

;;;; `before-minibuffer-hook', `after-minibuffer-hook'

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

;;;; `before-helm-hook', `after-helm-hook'

(defun run-before-helm-hook (&optional &rest args)
  (run-hooks 'before-helm-hook))
(defun run-after-helm-hook (&optional &rest args)
  (run-hooks 'after-helm-hook))

(add-hook 'helm-before-initialize-hook 'run-before-helm-hook)
(add-hook 'helm-exit-minibuffer-hook   'run-after-helm-hook)
(advice-add 'helm-keyboard-quit :after 'run-after-helm-hook)

;;; Operating systems
;;;; Fix selection on OS X

(when (eq system-type 'darwin)
  ;; Fix selections
  (defalias 'x-selection-owner-p 'ns-selection-owner-p))

;;; Lastly

(load custom-file)
