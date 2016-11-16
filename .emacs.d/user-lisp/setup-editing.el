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
(use-package clipmon                    ; Interact with system clipboard
  :init (clipmon-mode t)                ; Clipboard manager mode
  )
(use-package editorconfig               ; Normalized text style file format
  :init (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  :diminish (editorconfig-mode)
  )
(use-package expand-region)             ; Expand region by semantic units
(use-package focus)                     ; Dim outside thing at point (customize with M-x focus-mode-to-thing
;; FIXME Focus breaks ivy! Reported:
;; https://github.com/abo-abo/swiper/issues/755
;; FIXED, awaiting a release


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
                        default-cursor-type)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
)

(use-package highlight-indentation)     ; Show indent level markers
(use-package linum-relative             ; Relative line numbers
  :init (add-hook 'prog-mode-hook
                  'linum-on)
  :config (setq linum-relative-current-symbol ""
                linum-relative-with-helm nil)
  )
(use-package multiple-cursors
  :init
  (add-hook 'prog-mode-hook (lambda () (multiple-cursors-mode t)))
  (add-hook 'text-mode-hook (lambda () (multiple-cursors-mode t)))
  :bind (("C-S-c C-S-c" . mc/edit-lines))
  )
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
  :diminish (smartparens-mode)
  )

(use-package typo)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (setq
           undo-tree-auto-save-history t
           undo-tree-visualizer-diff t
           undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory  "/undo-forest" (number-to-string emacs-major-version))))
           )
  :diminish (undo-tree-mode)
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
  :diminish (yas-minor-mode)
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
