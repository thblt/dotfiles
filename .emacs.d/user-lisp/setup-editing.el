(require 'use-package)

;; Abbrev
(add-hook 'text-mode-hook (lambda ()
                            (abbrev-mode t)))
;;(add-hook 'c-common-mode '(abbrev-mode 0))

(use-package aggressive-indent)
(use-package auto-dictionary
  :init (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode)))
  )
(use-package anzu)                      ; Show matches count/current match # in mode line. Note: doesn't work with Swiper.
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

(use-package highlight-indentation)     ; Show indent level markers

(use-package linum-relative             ; Relative line numbers
  :init (add-hook 'prog-mode-hook
                  (lambda ()
                    (linum-relative-toggle)
                    ))
  :config (setq linum-relative-current-symbol ""))
(set-face-attribute 'linum t
                    :inherit 'default
                    :underline nil
                    :weight 'normal
                    )
;; Note: Nlinum is below

(use-package move-text ; Move text with M-[up|down]
  :init (move-text-default-bindings)
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

(use-package rainbow-delimiters)        ; Colorize parentheses etc by depth.
(use-package rainbow-mode  ; Preview colors codes and functions like "#ff0000", rgb(128,12,127)
  :diminish (rainbow-mode)
  :init (add-hook 'prog-mode-hook 'rainbow-mode t)
  )
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
  :config (progn
            (sp-pair "“" "”")
            (sp-pair "«" "»")
            (sp-local-pair 'org-mode "/" "/")
            (sp-local-pair 'org-mode "*" "*")
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

;; Compare buffer and file before warning about external modifications.
;; Ignore modification-time-only changes in files, i.e. ones that
;; don't really change the contents.  This happens often with
;; switching between different VC buffers.
;; Code from <http://stackoverflow.com/a/29556894>

(defun update-buffer-modtime-if-byte-identical ()
  (let* ((size      (buffer-size))
         (byte-size (position-bytes size))
         (filename  buffer-file-name))
    (when (and byte-size (<= size 1000000))
      (let* ((attributes (file-attributes filename))
             (file-size  (nth 7 attributes)))
        (when (and file-size
                   (= file-size byte-size)
                   (string= (buffer-substring-no-properties 1 (1+ size))
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (buffer-string))))
          (set-visited-file-modtime (nth 5 attributes))
          t)))))

(defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
  (or (funcall original buffer)
      (with-current-buffer buffer
        (update-buffer-modtime-if-byte-identical))))
(advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

(defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
  (unless (update-buffer-modtime-if-byte-identical)
    (apply original arguments)))
(advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supersession-threat--ignore-byte-identical)


;; Save everything on losing focus @TODO Make silent: disable "(No
;; files need saving)", autocreate directories when needed.
(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)
            )
          )

(provide 'setup-editing)
