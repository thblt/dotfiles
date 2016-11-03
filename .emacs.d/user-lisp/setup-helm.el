(require 'use-package)

(use-package helm
  :init (helm-mode t)
  :bind (
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         )
  )

(use-package swiper-helm)

(use-package helm-projectile
  :init (helm-projectile-on)
  )

(use-package helm-dash
     :bind (("<f1>" . helm-dash-at-point))
     )

;; (use-package counsel
;;   :bind (
;;          ("M-x" . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          )
;;   )
;; 
;; (use-package swiper-helm
;;   :bind (("C-s" . swiper))
;;   )
;; 
;; (use-package counsel-projectile
;;   :init (counsel-projectile-on)
;;   )

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

(provide 'setup-helm)
