(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
;;                         ("org" . "http://orgmode.org/elpa/"))

      ;; Emacs25+ only.  TODO This is an experiment: I'm not sure
      ;; putting GNU above Melpa unstable is a good idea: I don't know
      ;; if GNU packages are recent enough.  We'll see if something
      ;; breaks.
      package-archive-priorities '(
                                   ("melpa-stable" . 1000)
                                   ("gnu"          . 500)
                                   ("melpa"        . 100))
;;                                   ("org"          . 1))
				
      package-enable-at-startup nil

      ;; Isolate package directories for major Emacs version
      package-user-dir (concat user-emacs-directory "/elpa" (number-to-string emacs-major-version))
      )

(require 'package)
(package-initialize)

;; We need to configure and (require) package.el _before_ requiring
;; org and tangling dotemacs.org, or builtin org-mode will be loaded
;; instead of elpa version and updated versions will never be used.

(require 'org)
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
