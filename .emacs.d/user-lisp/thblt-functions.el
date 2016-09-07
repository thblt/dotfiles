(defun thblt/writer-mode ()
  "Selects a light theme, increase font size and activate variable-pitch-mode"
  (interactive)
  (progn
    (set-face-attribute 'default t :height 110)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'leuven)
    (variable-pitch-mode t)
    )
  )

(provide 'thblt-functions)
