;; =============================================================================
;; Basic preferences

(require 'subr-x)
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

(setf send-mail-function    #'smtpmail-send-it
      smtpmail-smtp-server  "mail.ankarstrom.se"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(setf gnus-message-archive-method mail-server
      gnus-message-archive-group  "Sent"
      gnus-gcc-mark-as-read       t)

(setf gnus-visible-headers
      (concat "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\"
              "|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\"
              "|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\"
              "|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\"
              "|^Resent-From:\\|^User-Agent:\\|^X-Mailer:"))

;; -----------------------------------------------------------------------------
;; Topics

(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

(setf gnus-topic-topology '(("Gnus" visible)
                            (("mail" visible nil nil))
                            (("lists" visible nil nil))
                            (("misc" invisible nil nil))))

;; TODO: Automatically divide groups into topics

;; -----------------------------------------------------------------------------
;; Formatting

;; Group display names

(setf gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")

(defun gnus-user-format-function-G (arg)
  (let* ((imap-prefix (format "nnimap+%s:" (plist-get mail-server 'nnimap)))
         (list-prefix (concat imap-prefix "Lists."))
         (name        gnus-tmp-group))
    (mapc (lambda (prefix)
            (setf name (string-remove-prefix prefix name)))
          (list list-prefix imap-prefix))
    (if (equal name "Lists")
        (setf name "(all)")) ; parent folder for lists
    name))

;; Summary

(setf gnus-summary-line-format "%1z%U%R %>%>%>%> %d.%* %f %[%L%] %s\n")
;; (setf gnus-summary-line-format "%1z%U%R %>%>%>%> %d.%* %f %[%L%] %s\n%ue\n")

(defun gnus-user-format-function-e (header)
  "Return article excerpt."
  "")

;; -----------------------------------------------------------------------------
;; Sorting

;; Sort summary buffer (best first)
(setf gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject            ; 1) a-z
        gnus-thread-sort-by-most-recent-number ; 2) highest number
        gnus-thread-sort-by-total-score))      ; 3) highest score

;; -----------------------------------------------------------------------------
;; Scoring

(setf gnus-use-adaptive-scoring t
      gnus-decay-scores t)

;; Increase group score on summary exit
(add-hook 'gnus-summary-exit-hook #'gnus-summary-bubble-group)
(setf gnus-group-sort-function #'gnus-group-sort-by-score)

;; Increase the score for followups to a sent article
(add-hook 'message-sent-hook #'gnus-score-followup-article)
(add-hook 'message-sent-hook #'gnus-score-followup-thread)

;; -----------------------------------------------------------------------------
;; Directories

;; NB: Be sure to set `gnus-init-file' in main Emacs configuration.

(setf gnus-home-directory         (gnus-dir)
      message-directory           (gnus-dir "mail/")
      gnus-directory              (gnus-dir "news/")
      nnfolder-directory          (gnus-dir "mail/archive/")
      gnus-startup-file           (gnus-dir "newsrc")
      gnus-home-score-file        (gnus-dir "gnus.SCORE")
      gnus-global-score-files     (list (gnus-dir "all.SCORE"))
      gnus-agent-directory        (gnus-dir "agent/")
      gnus-article-save-directory (gnus-dir "news")
      gnus-cache-directory        (gnus-dir "news/cache")
      gnus-cache-active-file      (gnus-dir "news/cache/active")
      gnus-kill-files-directory   (gnus-dir "news")
      nndraft-directory           (gnus-dir "drafts/"))

;; -----------------------------------------------------------------------------
;; Window configuration

(let ((group-width   35)
      (article-width 80))
  (gnus-add-configuration
   `(article
     (horizontal 1.0
                 (article ,article-width)
                 (summary 1.0 point))))

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
    ("cm" message-mark-inserted-region "Mark inserted region C-c M-m")
    ("q" nil "cancel"))
  (define-key message-mode-map (kbd "C-c v") 'hydra-message/body))
