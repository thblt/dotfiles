(require 'package)

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")                         
                         )

      package-archive-priorities '(
                                   ("melpa-stable" . 20)
                                   ("gnu"          . 15) 
                                   ("melpa"        . 10)
                                   ))
      ;; TODO This is an experiment: I'm not sure putting above melpa
      ;; unstable is a good idea: I don't know if GNU packages are
      ;; recent enough.  We'll see if something breaks.

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))                     ; Load package list if absent.

(eval-and-compile
  (package-install 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t)
  )

(provide 'setup-package-manager)
