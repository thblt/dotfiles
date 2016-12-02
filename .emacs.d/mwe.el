(setq package-user-dir (concat
			user-emacs-directory
			"/elpa"
			(number-to-string emacs-major-version)
			)
      )

(require 'package)
(package-initialize)

(require 'projectile)
(require 'counsel-projectile)
;; (ivy-mode)
(projectile-global-mode)
(counsel-projectile-on)

