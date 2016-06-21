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
            )
;;  :config (progn
			;; (setq evil-insert-state-cursor '(bar))
			;; ;; Ctrl belongs to Emacs's realm: use normal Emacs bindings.
			;; ;; @TODO Should be done using :bind or something, but I haven't
			;; ;; yet found how. (Seeing use-package docs, it seems it can't be
			;; ;; done. See color-moccur example in README.md)
			;; (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
			;; (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
			;; (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
			;; ; In insert mode, `evil-end-of-line' puts the cursor *before*
			;; ; the last character, which is useless.
			;; (define-key evil-insert-state-map "\C-e" 'end-of-line) 
			;; (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			;; (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			;; (define-key evil-insert-state-map "\C-k" 'kill-line)
			;; (define-key evil-insert-state-map "\C-n" 'evil-next-line)
			;; (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
			;; (define-key evil-insert-state-map "\C-w" 'evil-delete)
			;; (define-key evil-insert-state-map "\C-y" 'yank)
			;; (define-key evil-motion-state-map "\C-e" 'end-of-line)
			;; (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
			;; (define-key evil-normal-state-map "\C-b" 'evil-backward-char)
			;; (define-key evil-normal-state-map "\C-d" 'evil-delete-char)
			;; (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
			;; (define-key evil-normal-state-map "\C-f" 'evil-forward-char)
			;; (define-key evil-normal-state-map "\C-k" 'kill-line)
			;; (define-key evil-normal-state-map "\C-n" 'evil-next-line)
			;; (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
			;; (define-key evil-normal-state-map "\C-w" 'evil-delete)
			;; (define-key evil-normal-state-map "\C-y" 'yank)
			;; (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
			;; (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
			;; (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
			;; (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
			;; (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
			;; (define-key evil-visual-state-map "\C-k" 'kill-line)
			;; (define-key evil-visual-state-map "\C-n" 'evil-next-line)
			;; (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
			;; (define-key evil-visual-state-map "\C-w" 'evil-delete)
			;; (define-key evil-visual-state-map "\C-y" 'yank)
			;; )
  :init (progn
          (setq evil-disable-insert-state-bindings t) ;; Use Emacs bindings in insert mode. 
          
          ;; Restrict Evil to text-editing modes.
          ;; FIXME This won't work in Fundamental mode.
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
