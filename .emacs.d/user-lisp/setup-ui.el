(require 'use-package)

;; Themes

(use-package alect-themes
  :defer t
  )

(use-package zenburn-theme
  :defer t
  )

(load-theme 'zenburn)

;; Fonts

(setq thblt/base-font-size 090)
(when (string-prefix-p system-name "rudiger")
  (setq thblt/base-font-size 110)
  )

(setq default-frame-alist '((font . "DejaVu Sans Mono for Powerline")))
(set-face-attribute 'default t :height thblt/base-font-size)

;; UI elements

(unless (string= 'system-type 'darwin) (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;

(use-package hydra)

(use-package neotree                    ; FS sidebar à la NERDTree
  :bind ("<f2>" . neotree-toggle)
  )

(use-package windmove
  :init (windmove-default-keybindings)
  )

(defun startup-echo-area-message ()
  "I'm ready!") ; Because SpongeBob.

(provide 'setup-ui)
