(use-package powerline
  :config (setq powerline-default-separator 'wave)
  )

(defface powerline-modified-face
  '((((class color))
     (:background "#FFA335" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify modified files."
  :group 'powerline)

(defface powerline-normal-face
  '((((class color))
     (:background "#4F9D03" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify unchanged files."
  :group 'powerline)

(defface powerline-default-dictionary-active-face
  '((((class color))
     (:background "#8A2BE2" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify default dictionary in the active buffer."
  :group 'powerline)

(defface powerline-default-dictionary-inactive-face
  '((((class color))
     (:background "thistle" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify default dictionary in inactive buffers."
  :group 'powerline)

(defface powerline-other-dictionary-active-face
  '((((class color))
     (:background "yellow" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify another dictionary in the active buffer."
  :group 'powerline)

(defface powerline-other-dictionary-inactive-face
  '((((class color))
     (:background "LightYellow1" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify another dictionary in inactive buffers."
  :group 'powerline)

(defface powerline-buffer-position-face
  '((((class color))
     (:background "#D2D2D2" :foreground "#282828"))
    (t (:weight bold)))
  "Face to fontify buffer position."
  :group 'powerline)

(defun powerline-simpler-vc-mode (s)
  (if s
      (replace-regexp-in-string "\\(Git\\|SVN\\)[-:]" "" s)
    s))

(defun powerline-leuven-theme ()
  "Setup the leuven mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active
                                         'mode-line
                                       'mode-line-inactive))
                          (face1 (if active
                                     'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active
                                     'powerline-active2
                                   'powerline-inactive2))
                          (default-dictionary-face
                            (if active
                                'powerline-default-dictionary-active-face
                              'powerline-default-dictionary-inactive-face))
                          (other-dictionary-face
                           (if active
                               'powerline-other-dictionary-active-face
                             'powerline-other-dictionary-inactive-face))
                          (separator-left
                           (intern
                            (format "powerline-%s-%s"
                                    powerline-default-separator
                                    (car powerline-default-separator-dir))))
                          (separator-right
                           (intern
                            (format "powerline-%s-%s"
                                    powerline-default-separator
                                    (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; VC mode.
                                (when (and (fboundp 'vc-switches)
                                           buffer-file-name
                                           vc-mode)
                                  (if (eq (vc-state buffer-file-name) 'up-to-date)
                                      (powerline-simpler-vc-mode (powerline-vc 'powerline-normal-face 'r))
                                    (powerline-simpler-vc-mode (powerline-vc 'powerline-modified-face 'r))))

                                (when (and (not (fboundp 'vc-switches))
                                           buffer-file-name
                                           vc-mode)
                                  (powerline-simpler-vc-mode (powerline-vc face1 'r)))

                                (when (and buffer-file-name
                                           vc-mode)
                                  (if (eq (vc-state buffer-file-name) 'up-to-date)
                                      (funcall separator-left 'powerline-normal-face mode-line)
                                    (funcall separator-left 'powerline-modified-face mode-line)))

                                ;; "Modified" indicator.
                                (if (not (buffer-modified-p))
                                    (powerline-raw "%*" nil 'l)
                                  (powerline-raw "%*" 'mode-line-emphasis 'l))

                                (powerline-raw mode-line-mule-info nil 'l)

                                (powerline-buffer-id nil 'l)

                                (when (and (boundp 'which-func-mode) which-func-mode)
                                  (powerline-raw which-func-format nil 'l))

                                (powerline-raw " ")
                                (funcall separator-left mode-line face1)
                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                (powerline-minor-modes face2 'l)
                                (powerline-narrow face2 'l)
                                (powerline-raw " " face2)
                                (funcall separator-left face2 mode-line)))
                          (rhs (list (powerline-raw global-mode-string mode-line 'r)
                                     (funcall separator-right mode-line face1)

                                     (powerline-raw "%l," face1 'l)
                                     (powerline-raw "%c" face1 'r)
                                     (funcall separator-right face1 'powerline-buffer-position-face)
                                     (powerline-raw " %3p" 'powerline-buffer-position-face 'r)
                                     (funcall separator-right 'powerline-buffer-position-face face2)
                                     (powerline-buffer-size face2 'l)
                                     (powerline-raw " " face2)

                                     (let ((dict (and (featurep 'ispell)
                                                      (or
                                                       ispell-local-dictionary
                                                       ispell-dictionary))))
                                       ;; Add 2 spaces after the language indicator
                                       ;; (for GNU/Linux).
                                       (cond (buffer-read-only
                                              (powerline-raw "%%%%  " default-dictionary-face 'l))
                                             ((null dict)
                                              (powerline-raw "--  " default-dictionary-face 'l))
                                             (t
                                              (powerline-raw (concat (substring dict 0 2) "  ") other-dictionary-face 'l))))

                                     ;; (powerline-hud face2 face1)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

(with-eval-after-load "powerline-autoloads"
  (add-hook 'after-init-hook #'powerline-leuven-theme))

(use-package diminish
  :config (progn
            ;; Diminish modeline clutter
            (diminish 'abbrev-mode)
            (diminish 'buffer-face-mode)
            (diminish 'text-scale-mode)
            (diminish 'variable-pitch-mode)
            (diminish 'visual-line-mode)

            (eval-after-load "autorevert"
              '(diminish 'auto-revert-mode "↻"))
            
            (eval-after-load "company"
              '(diminish 'company-mode))

            (eval-after-load 'linum-relative
              '(diminish 'linum-relative-mode))
            
            (eval-after-load 'flycheck
              '(diminish 'flycheck-mode))
            
            (eval-after-load "flyspell"
              '(diminish 'flyspell-mode))

            (eval-after-load "org"
              '(diminish 'org-indent-mode))
            
            (eval-after-load "face-remap"
              '(diminish 'text-scale-mode))
                        )
  )

(provide 'setup-modeline)
