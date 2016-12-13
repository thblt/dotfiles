;;; This is the master init file.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
;;; Code:

;; Local load path
(progn
  (add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
  (add-to-list 'custom-theme-load-path (expand-file-name "user-lisp" user-emacs-directory))

  (add-to-list 'load-path "/home/thblt/Documents/Code/eziam-theme-emacs")
  (add-to-list 'custom-theme-load-path "/home/thblt/Documents/Code/eziam-theme-emacs"))

(require 'setup-package-manager)

;; Fundamentals: defaults, look and feel, general editing
(require 'init-defaults)
(require 'setup-ui)
;;(require 'setup-modeline)
(require 'setup-editing)
(require 'setup-misc)

;; General: common minor modes and utilities
(require 'setup-git)
(require 'setup-ivy)

;; Writing prose
(require 'setup-org)
(require 'setup-tex)

;; Writing code
(require 'setup-programming)
(require 'setup-projectile)

;; Languagues and major modes
(require 'setup-cfamily)
(require 'setup-haskell)
(require 'setup-lua)
(require 'setup-markdown)
(require 'setup-mu4e)
(require 'setup-python)
(require 'setup-webdev)
(require 'setup-yaml)

(require 'server)
(unless (server-running-p)
  (server-start)
  )

