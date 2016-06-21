(bind-key (kbd "<f5>") (lambda ()
                         (interactive)
                         (save-some-buffers t)
                         (recompile)
                         )
          )

(use-package company                    ; Completion framework
  :init (add-hook 'prog-mode-hook 'company-mode)
  )

(use-package flycheck                   ; On the fly checking/linting
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(provide 'setup-programming)
