;; OSX-specific configuration

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

(provide 'init-platform)
