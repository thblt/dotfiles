(require 'use-package)

(use-package nlinum ; More efficient line numbering, especially on large files with huge foldings (eg org)
  :config (nlinum-mode)
  )
(use-package linum-relative             ; Relative line numbers
;;  :init (linum-relative-global-mode)
  :config (setq linum-relative-current-symbol ""
                linum-relative-with-helm nil)
  )

;; Editing
(use-package anzu)                      ; Show matches count/current match # in mode line
(use-package avy                        ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
  :bind (("C-:" . avy-goto-char-timer)
         ("C-M-:" . avy-goto-char-timer)
         ("C-=" . avy-goto-line)
         )
  )
(use-package unfill
  :bind (
         ("M-Q" . unfill-paragraph)
         )
  )

(use-package undo-tree
	     :config (diminish 'undo-tree-mode)
	     )


(use-package expand-region)             ; Expand region by semantic units
(use-package highlight-indentation)     ; Show indent level markers
(use-package rainbow-delimiters)        ; Colorize parentheses etc by depth.
(use-package smartparens-config          ; Be smart with parentheses
  :ensure smartparens
  :init (progn
          (show-smartparens-global-mode t)
          )
  )
(use-package aggressive-indent)
(use-package smart-tabs-mode
;;  :init (add-hook 'c-mode-common-hook (lambda ()
;;                                    (smart-tabs-mode-enable)
;;                                    )
;;                  )
;;  :config (smart-tabs-insinuate 'c 'c++ 'python 'javascript)
  )

(use-package writeroom-mode)            ; Distraction-free mode
(use-package yasnippet                  ; Snippets
  :init (yas-global-mode)
  )

(provide 'setup-editing)
