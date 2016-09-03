(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'arrow)
  (spaceline-spacemacs-theme)
  )

(use-package diminish
  :config (progn
            ;; Diminish modeline clutter
            (diminish 'abbrev-mode)
            (diminish 'buffer-face-mode)
            (diminish 'text-scale-mode)
            (diminish 'variable-pitch-mode)
            (diminish 'visual-line-mode)

            (eval-after-load "autorevert"
              '(diminish 'auto-revert-mode "↻"))
            
            (eval-after-load "company"
              '(diminish 'company-mode))

            (eval-after-load 'linum-relative
              '(diminish 'linum-relative-mode))
            
            (eval-after-load 'flycheck
              '(diminish 'flycheck-mode))
            
            (eval-after-load "flyspell"
              '(diminish 'flyspell-mode))

            (eval-after-load "magit"
              '(diminish 'magit-mode))
            
            (eval-after-load "projectile"
              '(diminish 'projectile-mode))

            (eval-after-load "smartparens"
              '(diminish 'smartparens-mode))

            (eval-after-load "text-scale-mode"
              '(diminish 'text-scale-mode))
            
            (eval-after-load "undo-tree"
              '(diminish 'undo-tree-mode))
            
            (eval-after-load "yas-minor-mode"
              '(diminish 'yas-minor-mode))

            (eval-after-load "yasnippet"
              '(diminish 'yas-minor-mode))
            )
  )

(provide 'setup-modeline)
