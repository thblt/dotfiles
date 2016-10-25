(require 'use-package)

(use-package ivy
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
 
;; (use-package helm
;;   :init (helm-mode t)
;;   :bind (
;;          ("M-x"   . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          )
;;   )
;; 
;; (use-package helm-dash ; Access Dash docsets through Helm.
;;   :bind ("<f1>" . helm-dash-at-point)
;;   )
;; 
;; (use-package helm-projectile
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

(provide 'setup-ivy)
