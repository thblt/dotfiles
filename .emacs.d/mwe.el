(setq package-user-dir (concat
			user-emacs-directory
			"/elpa"
			(number-to-string emacs-major-version)
			)
      )

(package-initialize)

(require 'org)
