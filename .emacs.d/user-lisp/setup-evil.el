(require 'use-package)

(use-package evil                       ; Extensible VI Layer
  :config (progn
            (setq-default
             evil-insert-state-tag "INSERT"
             evil-normal-state-tag "NORMAL"
             evil-visual-state-tag "VISUAL"
             evil-emacs-state-tag "EMACS"
             evil-replace-state-tag "REPLACE"
             )
            (evil-set-initial-state 'mu4e-compose-mode 'insert)
            )
  :init (progn
          (setq evil-disable-insert-state-bindings t)  ;; Use Emacs bindings in insert mode. 
          
          ;; Restrict Evil to text-editing modes.
          (add-hook 'conf-mode-hook 'evil-local-mode)
          (add-hook 'text-mode-hook 'evil-local-mode)
          (add-hook 'prog-mode-hook 'evil-local-mode)
          )
  )

;; (use-package evil-leader)            ; Enable <leader> key 
(use-package evil-surround              ; A port of tpope's Surround
  :config (global-evil-surround-mode t)
  )
(use-package evil-nerd-commenter       ; A port of NerdCommenter
 ;; :config (evilnc-default-hotkeys)
  )

(provide 'setup-evil)
