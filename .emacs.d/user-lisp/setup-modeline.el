(use-package powerline
  :config (setq powerline-default-separator 'wave)
  )

(use-package spaceline-config
  :ensure spaceline
  :config (progn
            (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
            (spaceline-toggle-evil-state-on)
            (spaceline-toggle-flycheck-error-off)
            (spaceline-toggle-flycheck-warning-off)
            (spaceline-emacs-theme)
            )
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

            (eval-after-load "org"
              '(diminish 'org-indent-mode))

            (eval-after-load "face-remap"
              '(diminish 'text-scale-mode))
                        )
  )

(provide 'setup-modeline)
