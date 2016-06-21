(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  )

(provide 'setup-spaceline)
