(setq package-user-dir (concat
			user-emacs-directory
			"/elpa"
			(number-to-string emacs-major-version)
			)
      )

(package-initialize)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Complete below
