(require 'use-package)

(use-package tex-site
  :ensure auctex
  :init (add-hook 'LaTeX-mode-hook (progn
                                     'turn-on-flyspell
                                     'TeX-fold-mode
                                     )
                  ) 
  :config (progn
            (setq-default TeX-save-query nil      ; Autosave
                          TeX-parse-self t
                          TeX-engine 'xetex
                          )
            (setq TeX-view-program-list '(("MuPDF" "mupdf %s.pdf"))
                  TeX-view-program-selection '((output-pdf "MuPDF"))
                  )
            )
)           

(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")))))

(use-package company-auctex)            ; Completion provider for AucTeX

;; BibTeX
(use-package ebib                       ; BibTex editing *app* 
  :config (setq ebib-bibtex-dialect 'biblatex)
  )

(provide 'setup-tex)
