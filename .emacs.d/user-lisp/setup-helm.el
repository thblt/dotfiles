(require 'use-package)

(use-package helm
  :bind ("C-x C-b" . helm-buffers-list)
  )

(use-package helm-ag)  ; The silver searcher

(use-package helm-dash ; Access Dash docsets through Helm.
  :bind ("<f1>" . helm-dash-at-point)
  )

;; Helm Dash mode hooks

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local helm-dash-docsets '("C" "C++" "Qt"))
            )
          )

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Emacs Lisp"))
            )
          )

(add-hook 'haskell-mode-hook 
          (lambda ()
            (setq-local helm-dash-docsets '("Haskell"))
            )
          )

(add-hook 'html-mode-hook
          (lambda ()      
            (setq-local helm-dash-docsets '("HTML"))
            )
          )

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("JavaScript"))
            )
          )

(add-hook 'python-mode-hook (lambda ()
                              (setq-local helm-dash-docsets '("Python 2" "Python 3"))
                              ) 
          )

(provide 'setup-helm)
