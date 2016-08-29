;;; Code

(defun mu4e-message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))


(use-package mu4e-maildirs-extension)
(use-package mu4e
  :ensure nil                 ; Comes with mu, not on a Emacs package repo
  :bind ( ("<f12>" . mu4e-quit) )
  :init (progn
          (require 'mu4e-contrib)
          (mu4e-maildirs-extension)
          (setq mu4e-html2text-command 'mu4e-shr2text
           
                mu4e-maildir "~/.Mail/"
                mu4e-get-mail-command "offlineimap"
                mu4e-update-interval 60 ;; seconds
                message-send-mail-function 'smtpmail-send-it

                mu4e-view-show-images t
                mu4e-split-view 'vertical
                mu4e-hide-index-messages t

                mu4e-use-fancy-chars t
                mu4e-headers-attach-mark '("" . "")
                mu4e-headers-encrypted-mark '("" . "")
                mu4e-headers-list-mark '("" . "")
                mu4e-headers-new-mark '("" . "")
                mu4e-headers-read-mark '("" . "")
                mu4e-headers-replied-mark '("" . "↩")
                mu4e-headers-seen-mark '("" . "")
                mu4e-headers-unseen-mark '("" . "")                
                mu4e-headers-unread-mark '("" . "✱")
                mu4e-headers-signed-mark '("" . "")

                mu4e-headers-has-child-prefix '("+" . "└┬")
                mu4e-headers-first-child-prefix '("|" . "├")

                mu4e-headers-default-prefix '("" . "├")
                
                mu4e-headers-fields '(
                                      (:date       . 11)
                                      (:flags      . 3)
                                      (:from       . 25)                                      
                                      (:subject    . nil)
                                      )

                mu4e-context-policy 'pick-first
                mu4e-compose-context-policy 'ask
                mu4e-contexts
                `( ,(make-mu4e-context
                     :name "Namo"
                     :enter-func (lambda () (mu4e-message "Namo"))
                     :match-func (lambda (msg)
                                   (when msg
                                     (mu4e-message-maildir-matches msg "^/Namo/")))
                     :vars '( ( user-mail-address	     . "thibault@thb.lt" )
                              ( user-full-name	         . "Thibault Polge" )
                              ( mu4e-sent-folder        . "/Namo/Sent" )
                              ( mu4e-drafts-folder      . "/Namo/Drafts" )
                              ( mu4e-trash-folder       . "/Namo/Trash" )
                              ( smtpmail-local-domain   . "thb.lt" )
                              ( smtpmail-smtp-server    . "smtp.sfr.fr" )
                              ( smtpmail-stream-type    . starttls )
                              ( smtpmail-smtp-service   . 25 ) ))
                  
                   ,(make-mu4e-context
                     :name "P1"
                     :enter-func (lambda () (mu4e-message "P1"))
                     :match-func (lambda (msg)
                                   (when msg
                                     (mu4e-message-maildir-matches msg "^/P1/")))
                     :vars '(  ( user-mail-address	     . "thibault.polge@univ-paris1.fr"  )
                               ( user-full-name	         . "Thibault Polge" )
                               ( mu4e-sent-folder        . "/P1/sent-mail" )
                               ( mu4e-drafts-folder      . "/P1/Drafts" )
                               ( mu4e-trash-folder       . "/P1/Trash" )
                               ( smtpmail-local-domain   . "univ-paris1.fr" )
                               ( smtpmail-smtp-server    . "smtp.univ-paris1.fr" )
                               ( smtpmail-smtp-user      . "tpolge" )
                               ( smtpmail-stream-type    . tls )
                               ( smtpmail-smtp-service   . 465 ) 
                   )))
       
                mu4e-bookmarks `( ("m:/P1/INBOX OR m:/Namo/INBOX"
                                   "Global inbox"            ?i)
                                  
                                  ("flag:unread AND (m:/P1/INBOX OR m:/Namo/INBOX)"
                                   "Unread messages"         ?v)
                                  
                                  ("flag:flagged"
                                   "Flagged"                 ?f)
                                  ) )
          (add-hook 'mu4e-view-mode-hook (visual-line-mode t))
          )
  
  )

(bind-key "<f12>" 'mu4e)

(provide 'setup-mu4e)
