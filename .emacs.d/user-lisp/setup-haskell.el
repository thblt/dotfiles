(require 'use-package)

;; Haskell
(use-package haskell-mode)
(use-package company-ghc                ; Completion provider for Haskell
  :init (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  )               
(use-package flycheck-haskell           ; Haskell provider for Flycheck
  :init '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  )

(provide 'setup-haskell)
