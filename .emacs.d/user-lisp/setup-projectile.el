;; Project management
(use-package projectile                 ; Project management
  :config (setq
           projectile-globally-ignored-file-suffixes (append '(
                                                               ".un~"
                                                               ".~undo-tree~"
                                                               )
                                                             projectile-globally-ignored-files
                                                             )
           )
  :init (projectile-global-mode)
  )

(provide 'setup-projectile)
