(setq-default

 ;; Modeline
 column-number-mode t       ; Column number in modeline
 line-number-mode t         ; Line - - -

 ;; Indentation and general editing
 major-mode 'text-mode      ; Don't use fundamental mode for general text: it has no hooks.
 tab-width 4                ; Set tab stops
 indent-tabs-mode nil       ; Default to spaces for indent (smart tabs
                            ; on some syntaxes, see below)
 comment-empty-lines t      ; 
 reb-re-syntax 'string      ; String syntax for re-builder

 ;; Stuff for programming
 compile-command "wmake"    ; A small script which invokes the first
                            ; build system it can find instructions
                            ; for (in .dotfiles/bin)
 
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "setsid"
 browse-url-generic-args '("xdg-open")
 ;; setsid xdg-open prevents emacs from killing xdg-open before it
 ;; actually opened anything.  see
 ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
 
 ;; General interface improvements
 vc-follow-symlinks t       ; Always follow symlinks to
                            ; version-controlled files.
 use-dialog-box nil         ; Always use the minibuffer for prompts
 inhibit-startup-screen t   ; Skip the startup screens

 initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n"
 )

;;; === Sanity ===
(fset 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

;; Autosave and backups in /tmp/ 
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Let Customize put its mess elsewhere
(setq custom-file (concat user-emacs-directory "_customize.el"))
(load custom-file)

;; === Look and feel ===
(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)
            )
          ) ; Save everything on losing focus @TODO Make silent:
            ; disable "(No files need saving)", autocreate directories
            ; when needed.

(provide 'init-defaults)
