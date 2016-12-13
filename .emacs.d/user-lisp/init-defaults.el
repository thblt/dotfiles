(setq-default
 ;; Indentation and general editing
 major-mode 'text-mode      ; Don't use fundamental mode for general text: it has no hooks.
 tab-width 4                ; Set tab stops
 indent-tabs-mode nil       ; Default to spaces for indent
 comment-empty-lines nil    ;
 reb-re-syntax 'string      ; String syntax for re-builder.  See
                            ; advice at
                            ; <https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder>

 ;; ispell
 ispell-silently-savep t    ; Don't ask before saving modified dict
 
 ;; Programs
 path-to-ctags "/usr/bin/ctags"
 
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "setsid"
 browse-url-generic-args '("xdg-open")
 ;; setsid xdg-open prevents emacs from killing xdg-open before it
 ;; actually opened anything.  see
 ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open

 ;; General interface improvements
 cursor-type 'bar               ; Nicer cursor
 enable-recursive-minibuffers t ; Yeah recursion
 inhibit-startup-screen t       ; Skip the startup screens
 use-dialog-box nil             ; Always use the minibuffer for prompts
 vc-follow-symlinks t           ; Always follow symlinks to
                                ; version-controlled files.

 initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n"

 disabled-command-function nil ; Do not warn about "confusing" commands
 )

;;; Sanity
(fset 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

(setq backup-directory-alist                  ; Autosave and backups in /tmp/
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))

      ;; Let Customize put its mess elsewhere
      custom-file (concat user-emacs-directory "_customize.el")
      )
(load custom-file)

;; OS-X specific settings
(when (string= system-type 'darwin)
  ;; Don't use alt, cmd is meta
  (setq mac-option-modifier 'nil
        mac-command-modifier 'meta)

  ; Fix weird Apple keymap.on full-size kbs.
  (global-set-key (kbd "<help>") 'overwrite-mode)

  ; Fix load-path for mu4e (not sure this is still needed)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (use-package exec-path-from-shell

  ; Load PATH from shell in Cocoa
    :init (exec-path-from-shell-initialize)
    )
  )

(provide 'init-defaults)
