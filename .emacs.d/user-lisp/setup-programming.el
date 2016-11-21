
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
  :config (progn
            (set-face-attribute 'flycheck-warning nil
                                :underline '(:color "orange" :style wave))
            (set-face-attribute 'flycheck-error nil
                                :underline '(:color "red" :style wave))
            (set-face-attribute 'flycheck-fringe-error nil
                                :background nil
                                :foreground "red")
            (set-face-attribute 'flycheck-fringe-warning nil
                                :background "orange"
                                :foreground "white")
            )
  )

(provide 'setup-programming)
