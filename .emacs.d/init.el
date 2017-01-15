(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/"))

      ;; Emacs25+ only.  TODO This is an experiment: I'm not sure
      ;; putting GNU above Melpa unstable is a good idea: I don't know
      ;; if GNU packages are recent enough.  We'll see if something
      ;; breaks.
      package-archive-priorities '(
                                   ("org"          . 1001)
                                   ("melpa-stable" . 1000)
                                   ("gnu"          . 500)
                                   ("melpa"        . 100))

      package-enable-at-startup t

      ;; Isolate package directories for major Emacs version
      package-user-dir (concat user-emacs-directory "/elpa" (number-to-string emacs-major-version))
      )

(require 'package)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(eval-and-compile
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

(require 'use-package)

;; We need to install org /before/ tangling dotemacs.org, or it will never update.

(require 'org)
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))

;; Package.el wants this:
;; (package-initialize)
;; to be present in this file.
