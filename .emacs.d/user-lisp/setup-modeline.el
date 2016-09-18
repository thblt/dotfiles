(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator   'wave)

  ;; Segments

  (spaceline-compile "not-main"
                     '( ; Left
                       (evil-state :face highlight-face)
                       (projectile-root)
                       (remote-host)
                       (buffer-id)
                       (which-function)
                       (major-mode)
                       (minor-modes :when active)
                       )
                     '( ; Right
                       (version-control :when active)
                       (battery :when active)
                       )
                     ) 
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  )

(use-package diminish
  :config (progn
            ;; Diminish modeline clutter
            (diminish 'abbrev-mode)
            (diminish 'buffer-face-mode)
            (diminish 'text-scale-mode)
            (diminish 'variable-pitch-mode)
            (diminish 'visual-line-mode)

            (eval-after-load "anzu"
              '(diminish 'anzu-mode))
            
            (eval-after-load "autorevert"
              '(diminish 'auto-revert-mode "↻"))
            
            (eval-after-load "company"
              '(diminish 'company-mode))

            (eval-after-load "help"
              '(diminish 'helm-mode))
            
            (eval-after-load 'linum-relative
              '(diminish 'linum-relative-mode))
            
            (eval-after-load 'flycheck
              '(diminish 'flycheck-mode))
            
            (eval-after-load "flyspell"
              '(diminish 'flyspell-mode))

            (eval-after-load "helm-mode"
              '(diminish 'helm-mode))
            
            (eval-after-load "magit"
              '(diminish 'magit-mode))

            (eval-after-load "org"
              '(diminish 'org-indent-mode))
            
            (eval-after-load "projectile"
              '(diminish 'projectile-mode))

            (eval-after-load "smartparens"
              '(diminish 'smartparens-mode))

            (eval-after-load "face-remap"
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
