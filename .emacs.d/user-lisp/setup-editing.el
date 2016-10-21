(require 'use-package)

;; Abbrev
(add-hook 'text-mode-hook (lambda ()
                            (abbrev-mode t) ) )

(use-package aggressive-indent)
(use-package auto-dictionary
  :init (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode)))
  )
(use-package anzu)                      ; Show matches count/current match # in mode line
(use-package avy                        ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
  :bind (("C-:" . avy-goto-char-timer)
         ("C-M-:" . avy-goto-char-timer)
         ("C-=" . avy-goto-line)
         )
  )
(use-package editorconfig               ; Normalized text style file format
  :init (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  )
(use-package expand-region)             ; Expand region by semantic units
(use-package highlight-indentation)     ; Show indent level markers
(use-package linum-relative             ; Relative line numbers
  :init (add-hook 'prog-mode-hook
                  'linum-on)
  :config (setq linum-relative-current-symbol ""
                linum-relative-with-helm nil)
  )
(use-package multiple-cursors)
(use-package nlinum ; More efficient line numbering, especially on large files with huge foldings (eg org)
  :config (nlinum-mode)
  )

(eval-after-load "org"
  (progn
    (setq org-hide-leading-stars t
          org-catch-invisible-edits t ; Avoid editing folded contents
          org-hide-emphasis-markers t
          org-src-fontify-natively t  ; Syntax highlighting in src blocks. Kind of a lighter MMM.
          )
    (add-hook 'org-mode-hook (lambda ()
                               (org-indent-mode t)
                               (visual-line-mode t)
                               (flyspell-mode t)
                             )
              )
    )
  )

(use-package rainbow-delimiters)        ; Colorize parentheses etc by depth.
(use-package smart-tabs-mode
;;  :init (add-hook 'c-mode-common-hook (lambda ()
;;                                    (smart-tabs-mode-enable)
;;                                    )
;;                  )
;;  :config (smart-tabs-insinuate 'c 'c++ 'python 'javascript)
  )
(use-package smartparens-config         ; Be smart with parentheses
  :ensure smartparens
  :init (progn
          (smartparens-global-mode )
          )
  )

(use-package typo)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (progn (diminish 'undo-tree-mode)
                 (setq
                  undo-tree-auto-save-history t
                  undo-tree-visualizer-diff t
                  )
                 )
  )
(use-package unfill                      ; Unfill
  :bind (
         ("M-Q" . unfill-paragraph)
         )
  )
(use-package writeroom-mode)            ; Distraction-free mode
(use-package yasnippet                  ; Snippets
  :config (add-to-list 'yas-snippet-dirs "~./emacs.d/snippets/")
  :init (yas-global-mode)
  )
(use-package auto-yasnippet
  :bind ( ("H-w" . aya-create)
          ("H-y" . aya-open-line)
          )
  )

; Save everything on losing focus @TODO Make silent:
; disable "(No files need saving)", autocreate directories
; when needed.
(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)
            )
          )

(provide 'setup-editing)
