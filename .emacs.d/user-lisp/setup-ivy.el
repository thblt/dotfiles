(require 'use-package)

(use-package ivy
  :config (setq ivy-use-virtual-buffers t)
  :init (ivy-mode)
  :diminish (ivy-mode)
  )

(use-package counsel
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         )
  )

(use-package swiper
  :bind (("C-s" . swiper))
  )

(use-package counsel-projectile
  :init (counsel-projectile-on)
  )

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Boost" "C" "C++" "Qt"))
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

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Python 2" "Python 3"))
            )
          )

(provide 'setup-ivy)
