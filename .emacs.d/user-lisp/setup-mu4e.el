;;; Code

(defvar my-mu4e-account-alist
  '(("Namo"
     (mu4e-sent-folder "/Namo/Sent")
     (mu4e-drafts-folder "/Namo/Drafts")
     (user-mail-address "thibault@thb.lt")
     (smtpmail-default-smtp-server "smtp.sfr.fr")
     (smtpmail-local-domain "thb.lt")
     (smtpmail-smtp-user "thblt")
     (smtpmail-smtp-server "smtp.sfr.fr")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 25))
    ("P1"
     (mu4e-sent-folder "/P1/sent-mail")
     (mu4e-drafts-folder "/P1/Drafts")
     (user-mail-address "thibault.polge@univ-paris1.fr")
     (smtpmail-default-smtp-server "smtp.univ-paris1.fr")
     (smtpmail-local-domain "univ-paris1.fr")
     (smtpmail-smtp-user "tpolge")
     (smtpmail-smtp-server "smtp.univ-paris1.fr")
     (smtpmail-stream-type tls)
     (smtpmail-smtp-service 465)
     )
    )
  )

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(use-package mu4e
  :ensure nil                 ; Comes with mu, not on a Emacs package repo
  :init (progn
          (require 'mu4e-contrib)
          (setq mu4e-html2text-command 'mu4e-shr2text

                mu4e-view-show-images t
                mu4e-split-view 'vertical
           
                mu4e-maildir "~/.Mail/"

                mu4e-get-mail-command "offlineimap"
                mu4e-update-interval 60 ;; seconds
                
                mu4e-sent-folder "/P1/sent-mail"
                mu4e-drafts-folder "/P1/Drafts"
                mu4e-trash-folder "/P1/Trash"
                user-mail-address "thibault.polge@univ-paris1.fr"
                message-send-mail-function 'smtpmail-send-it
                smtpmail-default-smtp-server "smtp.univ-paris1.fr"
                smtpmail-local-domain "univ-paris1.fr"
                smtpmail-smtp-server "smtp.univ-paris1.fr"
                smtpmail-smtp-user "tpolge"
                smtpmail-stream-type 'tls
                smtpmail-smtp-service 465)
          
          (setq mu4e-bookmarks `( ("m:/P1/INBOX OR m:/Namo/INBOX"
                                   "Global inbox"            ?i)
                                  
                                  ("flag:unread AND (m:/P1/INBOX OR m:/Namo/INBOX)"
                                   "Unread messages"         ?v)
                                  
                                  ("flag:flagged"
                                   "Flagged"                 ?f)
                                  ) )

          (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
          )
  )

(provide 'setup-mu4e)
