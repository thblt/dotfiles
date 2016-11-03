(require 'use-package)

;; Themes
(use-package leuven-theme
  :pin melpa
  )
(use-package tao-theme :defer t)

;; (require 'leuven-dark-theme)
;; (enable-theme 'leuven-dark)
(load-theme 'leuven)

;; Fonts
(setq thblt/base-font-size 090)
(when (string-prefix-p  "rudiger" system-name)
  (setq thblt/base-font-size 120)
  )

(setq default-frame-alist '((font . "DejaVu Sans Mono")))
(set-face-attribute 'default t :height thblt/base-font-size)


;; Smooth(er) scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
    
      scroll-step 1 ;; keyboard scroll one line at a time
      )

;; UI elements

(unless (string= 'system-type 'darwin) (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package hydra)

(use-package recentf
  :init (recentf-mode)
  )

(use-package windmove
  :init (windmove-default-keybindings)
  )

(defun startup-echo-area-message ()
  "I'm ready!") ; Because SpongeBob.

(provide 'setup-ui)
