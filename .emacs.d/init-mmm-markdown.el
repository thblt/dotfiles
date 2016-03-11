(use-package mmm-mode
  :init (progn
          (setq mmm-global-mode 'maybe)

          (mmm-add-classes
           '((markdown-python
              :submode python-mode
              :front "^``` python[\n\r]+"
              :back "^```$")))
          )
  
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
  )
