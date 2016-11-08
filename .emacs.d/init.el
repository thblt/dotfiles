;;; This is the master init file.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path  (expand-file-name "user-lisp" user-emacs-directory))

(require 'setup-package-manager)

;; Fundamentals: defaults, look and feel, general editing
(require 'init-defaults)
(require 'setup-ui)
(require 'setup-modeline)
(require 'setup-editing)

;; General: common minor modes and utilities
;; (require 'setup-evil) ;; Don't be evil
(require 'setup-git)
(require 'setup-ivy)
;;(require 'setup-helm)

;; Languagues and major modes
(require 'setup-programming)

(require 'setup-cfamily)
(require 'setup-haskell)
(require 'setup-lua)
(require 'setup-markdown)
(require 'setup-mu4e)
(require 'setup-projectile)
(require 'setup-python)
(require 'setup-tex)
(require 'setup-webdev)
(require 'setup-yaml)

(require 'thblt-functions)

(require 'server)
(unless (server-running-p)
  (server-start)
  )

