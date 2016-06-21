(use-package diminish
  :config (progn
            ;; Diminish modeline clutter
            (diminish 'abbrev-mode)
            
            (eval-after-load "smartparens"
              '(diminish 'smartparens-mode))
            
            (eval-after-load "undo-tree"
              '(diminish 'undo-tree-mode))
            
            (eval-after-load "projectile"
              '(diminish 'projectile-mode))

            (eval-after-load "yas-minor-mode"
              '(diminish 'yas-minor-mode))

            (eval-after-load "auto-complete"
              '(diminish 'auto-complete-mode))
            
            (eval-after-load "yasnippet"
              '(diminish 'yas-minor-mode))

            (eval-after-load "flyspell"
              '(diminish 'flyspell-mode))

            (eval-after-load "eldoc"
              '(diminish 'eldoc-mode))

            (eval-after-load "git-gutter"
              '(diminish 'git-gutter-mode))

            (eval-after-load "whitespace-cleanup-mode"
              '(diminish 'whitespace-cleanup-mode))

            (eval-after-load 'flycheck
              '(diminish 'flycheck-mode))
            
            (eval-after-load 'auto-revert-mode
              '(diminish 'auto-revert-mode))
            
            (eval-after-load 'magit-wip
              '(diminish 'magit-wip-save-mode))
            )
  )

(provide 'setup-diminish)
