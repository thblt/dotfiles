(require 'package)

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))

      ;; Emacs25+ only.  TODO This is an experiment: I'm not sure
      ;; putting GNU above Melpa unstable is a good idea: I don't know
      ;; if GNU packages are recent enough.  We'll see if something
      ;; breaks.
      package-archive-priorities '(
                                   ("melpa-stable" . 1000)
                                   ("gnu"          . 500)
                                   ("melpa"        . 100)
                                   )

      ;; Isolate package directories for major Emacs version
      package-user-dir (concat user-emacs-directory "/elpa" (number-to-string emacs-major-version))
      )

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))                     ; Load package list if absent.

(eval-and-compile
  (package-install 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t)
  )

(provide 'setup-package-manager)
