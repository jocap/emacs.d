(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(comment-column 0)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("ed4f603e56aa85f6885b86de49394eda021d38f4768671b8b27bacc06f5a5fa1" "228b21af874cf3ba1df3d6a684a63e33089aa200ad2ab65df747ffaca6b43419" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "3cddc1775f6c26573a69315dacd5fd45a6cd04df539b6354281d316985f254f3" "74dd165fb35d6605d833cb756250a95134d3bdf8342689866308e14a8891d092" "b85fc9f122202c71b9884c5aff428eb81b99d25d619ee6fde7f3016e08515f07" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(debug-on-error t)
 '(default-frame-alist (quote ((font . "Fira Mono Medium-10"))))
 '(default-input-method "russian-computer")
 '(eziam-scale-headings nil)
 '(eziam-scale-other nil)
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(fringe-mode nil nil (fringe))
 '(global-hl-line-mode nil)
 '(helm-autoresize-min-height 0)
 '(helm-autoresize-mode t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-external-programs-associations (quote (("html" . "firefox"))))
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(helm-split-window-in-side-p t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary-alist nil)
 '(linum-format "%4d │ ")
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 5) ((control)))))
 '(next-screen-context-lines 6)
 '(nlinum-format "%4d │")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("/mnt/c/Users/JohnAJ/Dropbox/org/index.org" "/mnt/c/Users/JohnAJ/Dropbox/org/life/services.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-babel-no-eval-on-ctrl-c-ctrl-c t)
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-separator-lines 2)
 '(org-directory "~/dropbox/org")
 '(org-edit-src-content-indentation 0)
 '(org-hide-emphasis-markers t)
 '(org-log-done (quote time))
 '(org-pretty-entities t)
 '(org-replace-disputed-keys t)
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (pcomplete-extension git-gutter+ buffer-move org-autolist org helm-org-rifle ox-twbs eziam-theme popwin nlinum openwith org-plus-contrib htmlize ace-link s zenity-color-picker helm-projectile term-projectile dash helm diminish use-package dashboard flx-ido restart-emacs fill-column-indicator iy-go-to-char magit avy paredit paredit-everywhere auctex auctex-latexmk pcre2el wrap-region solarized-theme visual-regexp-steroids vimrc-mode subatomic256-theme rainbow-delimiters origami multiple-cursors markdown-mode gruvbox-theme expand-region)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(solarized-scale-org-headlines nil)
 '(solarized-use-variable-pitch nil)
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(truncate-lines t)
 '(use-package-always-ensure t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell t)
 '(vr/engine (quote pcre2el))
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold-italic ((t (:slant italic :weight bold :family "Fira Mono Medium"))))
 '(font-lock-comment-face ((t (:foreground "#888888" :slant italic :family "Fira Mono Medium"))))
 '(italic ((t (:slant italic :family "Fira Mono Medium"))))
 '(magit-diff-conflict-heading ((t (:inherit magit-diff-hunk-heading :distant-foreground "dim gray"))))
 '(magit-section-highlight ((t (:background "#eee8d5" :distant-foreground "dim gray"))) nil "Made text visible.")
 '(relative-line-numbers ((t (:inherit (linum default)))))
 '(relative-line-numbers-current-line ((t (:inherit relative-line-numbers))))
 '(trailing-whitespace ((t (:background "#be4848"))))
 '(variable-pitch ((t (:height 110 :family "FreeSans"))))
 '(widget-field ((t (:background "#eee8d5" :foreground "dim gray"))) nil "Made text visible."))
