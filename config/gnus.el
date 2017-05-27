;; =============================================================================
;; Basic preferences

(setq user-mail-address "john@ankarstrom.se"
      user-full-name "John Ankarström")

(setq gnus-select-method '(nnnil))

(setq my-server '(nnimap "mail.ankarstrom.se"
                  (nnimap-address "mail.ankarstrom.se")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl)))

(setq gnus-secondary-select-methods (list my-server))

(setq nnmail-expiry-target "nnimap+mail.ankarstrom.se:Trash"
      nnmail-expiry-wait   'immediate)

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "mail.ankarstrom.se"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(setq gnus-message-archive-method my-server
      gnus-message-archive-group "Sent")

(setq gnus-gcc-mark-as-read t)

;; -----------------------------------------------------------------------------
;; Topics

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-topic-topology '(("Gnus" visible)
                            (("mail" visible nil nil))
                            (("misc" invisible nil nil))))

(setq gnus-topic-alist '(("mail"
                          "nnimap+mail.ankarstrom.se:INBOX"
                          "nnimap+mail.ankarstrom.se:Lists"
                          "nnimap+mail.ankarstrom.se:Archive"
                          "nnimap+mail.ankarstrom.se:Drafts"
                          "nnimap+mail.ankarstrom.se:Sent"
                          "nnimap+mail.ankarstrom.se:Trash"
                          "nnimap+mail.ankarstrom.se:Junk")
                         ("misc"
                          "nndraft:drafts"
                          "nndraft:queue")
                         ("Gnus")))

;; -----------------------------------------------------------------------------
;; Group display names

(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
    (if (null mapped-name)
        gnus-tmp-group
      (cdr mapped-name))))

(setq group-name-map '(("nnimap+mail.ankarstrom.se:INBOX"   . "Inbox")
                       ("nnimap+mail.ankarstrom.se:Archive" . "Archive")
                       ("nnimap+mail.ankarstrom.se:Drafts"  . "Drafts")
                       ("nnimap+mail.ankarstrom.se:Sent"    . "Sent")
                       ("nnimap+mail.ankarstrom.se:Trash"   . "Trash")
                       ("nnimap+mail.ankarstrom.se:Junk"    . "Junk")
                       ("nnimap+mail.ankarstrom.se:Lists"   . "Lists")))

;; -----------------------------------------------------------------------------
;; Window configuration

(gnus-add-configuration
 '(article
   (horizontal 1.0
    (group 25)
    (summary 1.0 point)
    (article 80))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
    (vertical 25
     (group 1.0))
    (vertical 1.0
     (summary 1.0 point)))))

;; =============================================================================
;; Hydra configuration courtesy of Chen Bin

;; (See https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org)

(eval-after-load 'gnus-group
  '(progn
    (defhydra hydra-gnus-group (:color blue)
      "Do"
      ("a" gnus-group-list-active "REMOTE groups A A")
      ("l" gnus-group-list-all-groups "LOCAL groups L")
      ("c" gnus-topic-catchup-articles "Read all c")
      ("G" gnus-group-make-nnir-group "Search server G G")
      ("g" gnus-group-get-new-news "Refresh g")
      ("s" gnus-group-enter-server-mode "Servers")
      ("m" gnus-group-new-mail "Compose m OR C-x m")
      ("#" gnus-topic-mark-topic "mark #")
      ("q" nil "cancel"))
    (define-key gnus-group-mode-map "v" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
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
    (define-key gnus-summary-mode-map "v" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
    (defhydra hydra-gnus-article (:color blue)
      "Do"
      ("f" gnus-summary-mail-forward "Forward")
      ("R" gnus-article-reply-with-original "Reply with original R")
      ("r" gnus-article-reply "Reply r")
      ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
      ("o" gnus-mime-save-part "Save attachment at point o")
      ("w" gnus-article-wide-reply "Reply all S w")
      ("q" nil "cancel"))
    (define-key gnus-article-mode-map "v" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
    (defhydra hydra-message (:color blue)
      "Do"
      ("ca" mml-attach-file "Attach C-c C-a")
      ("cc" message-send-and-exit "Send C-c C-c")
      ("q" nil "cancel"))
    (global-set-key (kbd "C-c v") 'hydra-message/body)))
