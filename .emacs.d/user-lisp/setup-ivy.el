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

(use-package counsel-dash
  :bind ("<f1>" . counsel-dash-at-point)
  :config
  (setq helm-dash-docsets-path "~/.local/share/DashDocsets")
  (defun counsel-dash-at-point ()
            (interactive)
            (counsel-dash (thing-at-point 'symbol))
            )
  )

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("C"))
            )
          )

(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Boost" "C++" "Qt"))
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
