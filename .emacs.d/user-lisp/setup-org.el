;;; Code:

(use-package org
  :pin org
  :init (progn
          (setq org-catch-invisible-edits t ; Avoid editing folded contents
                org-hide-leading-stars t
                org-hide-emphasis-markers t
                org-src-fontify-natively t  ; Syntax highlighting in src blocks.
                )
          (add-hook 'org-mode-hook (lambda ()
                                     (flyspell-mode t)
                                     (org-indent-mode t)
                                     (visual-line-mode t)
                                     )))
  :diminish org-indent-mode)


(use-package ox-reveal)

(use-package toc-org
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(provide 'setup-org)
