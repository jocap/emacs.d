; Miscellanous

(server-start) ; Start in server mode (launch using emacsclient)
(setq backup-directory-alist `(("." . "~/.saves"))) ; Saves ~ files to ~/.saves instead of current directory

; Packages

(add-to-list 'load-path "~/.emacs.d/")
(load-file "personal.el")
(load-file "markdown-mode.el")
