
(setq compile-command "wmake" ; A small script which invokes the first
                              ; build system it can find instructions
                              ; for (in .dotfiles/bin)
      )

(define-key prog-mode-map (kbd "<f8>") 'ffap)

(use-package company                    ; Completion framework
  :init (add-hook 'prog-mode-hook 'company-mode)
  )

(use-package flycheck                   ; On the fly checking/linting
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(provide 'setup-programming)
