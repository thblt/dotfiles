(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'alternate)
  (spaceline-spacemacs-theme)
  )


(provide 'setup-spaceline)
