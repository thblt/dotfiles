(require 'use-package)

(use-package clang-format)              ; Interface to clang-format
(use-package company-c-headers)         ; Completion provider for C header file
(use-package cpputils-cmake)            ; Automatic configuration for Flycheck/Company/etc for CMake projects
(use-package irony
  :diminish irony-mode
  )
(use-package flycheck-irony)
(use-package company-irony)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; From irony docs
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point]
;;    'irony-completion-at-point-async)
;;  (define-key irony-mode-map [remap complete-symbol]
;;    'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (irony-mode t)
            )
          )

;; CMake
(use-package cmake-mode)

(provide 'setup-cfamily)
