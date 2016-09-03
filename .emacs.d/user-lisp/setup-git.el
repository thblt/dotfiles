;; Versioning and history
(use-package git-timemachine
  :config   (progn
              (evil-make-overriding-map git-timemachine-mode-map 'normal)
              ;; force update evil keymaps after git-timemachine-mode loaded
              (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps) ) )

(use-package magit
  :bind ( ("C-x g" . magit-status) )
  )
;; General programming

(provide 'setup-git)
