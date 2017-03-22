; Miscellanous

(server-start) ; Start in server mode (launch using emacsclient)
(setq backup-directory-alist `(("." . "~/.saves"))) ; Saves ~ files to ~/.saves instead of current directory

; Packages & themes

(let ((default-directory "~/.emacs.d/packages"))
  (load-file (expand-file-name "personal.el"))
;  (load-file (expand-file-name "automode.el"))
;  (load-file (expand-file-name "pasta.el"))
  (load-file (expand-file-name "markdown-mode.el")))

; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; currently not used

; Auto mode

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) ; needed? is it working?
