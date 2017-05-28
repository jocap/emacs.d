;; =============================================================================
;; Basic preferences

(require 's)
(require 'dash)

(setf user-mail-address "john@ankarstrom.se"
      user-full-name "John Ankarström")

(setf gnus-select-method '(nnnil))

(setf mail-server '(nnimap "mail.ankarstrom.se"
                    (nnimap-address "mail.ankarstrom.se")
                    (nnimap-server-port "imaps")
                    (nnimap-stream ssl)))

(setf gnus-secondary-select-methods (list mail-server))

;; Use expire function to archive mail
(setf nnmail-expiry-target "nnimap+mail.ankarstrom.se:Archive"
      nnmail-expiry-wait   'immediate)

(setf send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "mail.ankarstrom.se"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(setf gnus-message-archive-method mail-server
      gnus-message-archive-group "Sent")

(setf gnus-gcc-mark-as-read t)

;; -----------------------------------------------------------------------------
;; Topics

(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

(setf gnus-topic-topology '(("Gnus" visible)
                            (("mail" visible nil nil))
                            (("lists" visible nil nil))
                            (("misc" invisible nil nil))))

;; -----------------------------------------------------------------------------
;; Group display names

(setf gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")

(defun gnus-user-format-function-G (arg)
  (let* ((prefix      (format "nnimap+%s:" (plist-get mail-server 'nnimap)))
         (list-prefix (concat prefix "Lists."))
         (name        gnus-tmp-group))
    (--map (setf name (s-chop-prefix it name))
           (list list-prefix prefix))
    (if (equal name "Lists")
        (setf name "(all)")) ; parent folder for lists
    name))

;; -----------------------------------------------------------------------------
;; Scoring & sorting

;; Increase group score on summary exit
(add-hook 'gnus-summary-exit-hook #'gnus-summary-bubble-group)
(setf gnus-group-sort-function #'gnus-group-sort-by-score)

;; Sort summary buffer (best first)
(setf gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject            ; 1) a-z
        gnus-thread-sort-by-most-recent-number ; 2) highest number
        gnus-thread-sort-by-total-score))      ; 3) highest score

;; -----------------------------------------------------------------------------
;; Directories

;; NB: Be sure to set `gnus-startup-file' in main Emacs configuration.

(setf gnus-home-directory         (gnus-dir)
      message-directory           (gnus-dir "mail/")
      gnus-directory              (gnus-dir "news/")
      nnfolder-directory          (gnus-dir "mail/archive/")
      gnus-init-file              (gnus-dir "gnus.el")
      gnus-home-score-file        (gnus-dir "gnus.SCORE")
      gnus-agent-directory        (gnus-dir "agent/")
      gnus-article-save-directory (gnus-dir "news")
      gnus-cache-directory        (gnus-dir "news/cache")
      gnus-cache-active-file      (gnus-dir "news/cache/active")
      gnus-kill-files-directory   (gnus-dir "news")
      nndraft-directory           (gnus-dir "drafts/"))

;; -----------------------------------------------------------------------------
;; Window configuration

(let ((group-width   40)
      (article-width 80))
  (gnus-add-configuration
   `(article
     (horizontal 1.0
                 (group ,group-width)
                 (summary 1.0 point)
                 (article ,article-width))))

  (gnus-add-configuration
   `(summary
     (horizontal 1.0
                 (vertical ,group-width
                           (group 1.0))
                 (vertical 1.0
                           (summary 1.0 point))))))

;; =============================================================================
;; Hydra configuration courtesy of Chen Bin

;; (See https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org)

(with-eval-after-load 'gnus-group
  (defhydra hydra-gnus-group (:color blue)
    "Group"
    ("a" gnus-group-list-active "REMOTE groups A A")
    ("l" gnus-group-list-all-groups "LOCAL groups L")
    ("c" gnus-topic-catchup-articles "Read all c")
    ("G" gnus-group-make-nnir-group "Search server G G")
    ("g" gnus-group-get-new-news "Refresh g")
    ("s" gnus-group-enter-server-mode "Servers")
    ("m" gnus-group-new-mail "Compose m OR C-x m")
    ("#" gnus-topic-mark-topic "mark #")
    ("q" nil "cancel"))
  (define-key gnus-group-mode-map "v" 'hydra-gnus-group/body))

;; gnus-summary-mode
(with-eval-after-load 'gnus-sum
  (defhydra hydra-gnus-summary (:color blue)
    "Do"
    ("s" gnus-summary-show-thread "Show thread")
    ("h" gnus-summary-hide-thread "Hide thread")
    ("n" gnus-summary-insert-new-articles "Refresh / N")
    ("f" gnus-summary-mail-forward "Forward C-c C-f")
    ("!" gnus-summary-tick-article-forward "Mail -> disk !")
    ("p" gnus-summary-put-mark-as-read "Mail <- disk")
    ("c" gnus-summary-catchup-and-exit "Read all c")
    ("e" gnus-summary-resend-message-edit "Resend S D e")
    ("R" gnus-summary-reply-with-original "Reply with original R")
    ("r" gnus-summary-reply "Reply r")
    ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
    ("w" gnus-summary-wide-reply "Reply all S w")
    ("#" gnus-topic-mark-topic "mark #")
    ("q" nil "cancel"))
  (define-key gnus-summary-mode-map "v" 'hydra-gnus-summary/body))

;; gnus-article-mode
(with-eval-after-load 'gnus-art
  (defhydra hydra-gnus-article (:color blue)
    "Do"
    ("f" gnus-summary-mail-forward "Forward")
    ("R" gnus-article-reply-with-original "Reply with original R")
    ("r" gnus-article-reply "Reply r")
    ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
    ("o" gnus-mime-save-part "Save attachment at point o")
    ("w" gnus-article-wide-reply "Reply all S w")
    ("q" nil "cancel"))
  (define-key gnus-article-mode-map "v" 'hydra-gnus-article/body))

(with-eval-after-load 'message
  (defhydra hydra-message (:color blue)
    "Do"
    ("ca" mml-attach-file "Attach C-c C-a")
    ("cc" message-send-and-exit "Send C-c C-c")
    ("q" nil "cancel"))
  (global-set-key (kbd "C-c v") 'hydra-message/body))
