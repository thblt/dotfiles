(require 'use-package)

;; Haskell
(use-package haskell-mode)
(use-package company-ghc)               ; Completion provider for Haskell
(use-package flycheck-haskell)          ; Haskell provider for Flycheck
(use-package helm-hoogle)               ; Search Hoogle 

(provide 'setup-haskell)
