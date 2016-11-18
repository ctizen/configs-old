;; ------------------------------------------------------------------------
;; mu4e mailer setup

(add-to-list 'load-path "~/.emacs.d/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary (concat
                      "~/.emacs.d/mu4e/mu-binary-"
                      (if (eq system-type 'darwin) "mac" "linux")))

;; default
(setq mu4e-maildir "~/Mail")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; enable inline images
(setq mu4e-view-show-images t)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX" . ?i)
         ("/Sent" . ?s)
         ("/Trash" . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "~/.emacs.d/fetch.sh"
      mu4e-update-interval 900
      )

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

(add-to-list 'mu4e-bookmarks
  '("flag:flagged"       "Important (flagged)"     ?f))

;; Defaults for new messages
(setq user-mail-address "me@ctizen.net"
      user-full-name  "Oleg Klimenko"
      mu4e-compose-signature
      (concat
        "Oleg Klimenko\n"
        "Email: " user-mail-address "\n"
        "Mobile: +7 913 725 6906\n"
        "\n"))

;; For replies we set corresponding "from:"
(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
	(setq user-mail-address
	  (cond
	    ((mu4e-message-contact-field-matches msg :to "o.klimenko@2gis.ru")
             "o.klimenko@2gis.ru")
	    ((mu4e-message-contact-field-matches msg :to "heilage.nsk@gmail.com")
             "heilage.nsk@gmail.com")
            ((mu4e-message-contact-field-matches msg :to "klimenkoov@gmail.com")
             "klimenkoov@gmail.com")
	    (t "me@ctizen.net")))))
    (setq
     user-full-name  "Oleg Klimenko"
     mu4e-compose-signature
     (concat
      "Oleg Klimenko\n"
      "Email: " user-mail-address "\n"
      "Mobile: +7 913 725 6906\n"
      "\n"))
    
    ))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'ssl
    mail-host-address "ctizen.net"
    smtpmail-default-smtp-server "smtp.zoho.com"
    smtpmail-smtp-server "smtp.zoho.com"
    smtpmail-smtp-service 465)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq mu4e-headers-fields
   (quote
    ((:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:maildir . 10)
     (:from . 22)
     (:subject))))
