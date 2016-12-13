(setq compile-command "wmake" ; A small script which invokes the first
                              ; build system it can find instructions
                              ; for (in .dotfiles/bin)
      )

(define-key prog-mode-map (kbd "<f8>") 'ffap)

(use-package evil-nerd-commenter
  :bind (("M-;"   . evilnc-comment-or-uncomment-lines)
         ("C-M-;" . evilnc-comment-or-uncomment-paragraphs)
         ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
         ("C-c c" . evilnc-copy-and-comment-lines)
         ("C-c p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package company                    ; Completion framework
  :init (add-hook 'prog-mode-hook 'company-mode)
  :diminish company-mode
  )

(use-package flycheck                   ; On the fly checking/linting
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :diminish flycheck-mode
  )

(provide 'setup-programming)
