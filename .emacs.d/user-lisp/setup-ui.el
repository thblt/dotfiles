(require 'use-package)

;; Themes
(use-package monokai-theme   :defer t)
(use-package solarized-theme :defer t)
(use-package spacemacs-theme :defer t)
(use-package zenburn-theme   :defer t)

(load-theme 'spacemacs-dark)

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

(use-package god-mode
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
         ("z"        . repeat)
         ("i"        . god-local-mode)
         )
  :config
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

  )

(use-package hydra)

(use-package neotree                    ; FS sidebar à la NERDTree
  :bind ("<f2>" . neotree-toggle)
  )

(use-package recentf
  :init (recentf-mode)
  )

(use-package windmove
  :init (windmove-default-keybindings)
  )

(defun startup-echo-area-message ()
  "I'm ready!") ; Because SpongeBob.

(provide 'setup-ui)
