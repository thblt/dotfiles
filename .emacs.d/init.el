;;; This is the master init file.

(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))

(require 'setup-package-manager)

;; Fundamentals: defaults, look and feel, general editing
(require 'init-defaults)
(require 'setup-ui)
(require 'setup-editing)

;; General: common minor modes and utilities
(require 'setup-evil)
(require 'setup-helm)
(require 'setup-modeline)

;; Languagues and major modes
(require 'setup-programming)

(require 'setup-cfamily)
(require 'setup-haskell)
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
