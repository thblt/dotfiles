(setq package-user-dir (concat
			user-emacs-directory
			"/elpa"
			(number-to-string emacs-major-version)
			)
      )

(require 'package)
(package-initialize)

(require 'counsel)
(require 'highlight-indent-guides)

(ivy-mode)

