(require 'package)

;; oooooooooo.    o8o              ooooo ooooo      ooo oooooooooooo 
;; `888'   `Y8b   `"'              `888' `888b.     `8' `888'     `8 
;;  888      888 oooo  oooo    ooo  888   8 `88b.    8   888         
;;  888      888 `888   `88.  .8'   888   8   `88b.  8   888oooo8    
;;  888      888  888    `88..8'    888   8     `88b.8   888    "    
;;  888     d88'  888     `888'     888   8       `888   888       o 
;; o888bood8P'   o888o     `8'     o888o o8o        `8  o888ooooood8
;;
;; DivINE · is · not · Evil
;;
;; DivINE is “normal mode for Emacs”.  It tries to bring modal editing
;; to Emacs by adding a “native” normal mode.  Unlike Evil and similar
;; packages, it does *not* try to recreate Vim's features and behavior
;; (although it is not too far them) but to give Emacs users the
;; benefit of text editing without modifier keys and induced injuries.
;;
;; It currently differs from Vim's normal mode in the following way:
;;
;;  - No visual modes at all.  Text selection is performed in normal
;;    mode.  <Space> activates or deactivates the mark.
;;
;;  - y and p are reversed: On Emacs, to yank is to *paste*, not copy.
;;    y puts the contents of the clipboard at point, where p<text
;;    object> actually copies some text to the kill ring.  To remember
;;    this: p is the only hard consonant in coPy (or if you're French,
;;    think «presse-papier»).  k works the same way as in Vim.
;;
;;  - Text units are understood the Emacs way.  We don't even try to
;;    emulate Vim's understanding of what a word, a sentence... is.
                                                                  
(use-package god-mode
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
         ("^" . beginning-of-line)
         ("$" . end-of-line)         
         ("z" . repeat)
         ("i" . god-local-mode)
         )
  
  :config (progn
            (defun my-update-cursor ()
              (setq cursor-type (if (or god-local-mode buffer-read-only)
                                    'box
                                  default-cursor-type)))

            (add-hook 'god-mode-enabled-hook 'my-update-cursor)
            (add-hook 'god-mode-disabled-hook 'my-update-cursor)
            )
  )

(require 'god-mode)
(require 'hydra)
(require 'thingatpt)


(provide 'setup-god)
