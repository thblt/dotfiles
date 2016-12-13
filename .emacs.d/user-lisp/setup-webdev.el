(use-package emmet-mode)                ; "Zen" XML
(use-package haml-mode)                 ; HAML templates
(use-package less-css-mode)             ; LESS
(use-package scss-mode
  :init (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode)))
(use-package skewer-mode)               ; JSFiddle/CodePen like mode
(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))))

(provide 'setup-webdev)
