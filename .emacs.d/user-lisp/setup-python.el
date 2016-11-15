(use-package company-jedi
  :config (add-hook 'python-mode-hook (progn
                                        (add-to-list 'company-backends 'company-jedi)
                                        )
                    )
  )

(use-package flycheck-pyflakes)         ; Pyflakes provider for Flycheck

(add-hook 'python-mode-hook (lambda ()
                              ;; Configure indentation
                              (setq-default indent-tabs-mode t)
                              (setq-default tab-width 4)
                              (setq-default py-indent-tabs-mode t)
                              (add-to-list 'write-file-functions 'delete-trailing-whitespace)
                              ;; Guess tab style for existing code
                              (guess-style-guess-tab-width)
                              ;; Enable Jedi completion
                              )
          )

(provide 'setup-python)
