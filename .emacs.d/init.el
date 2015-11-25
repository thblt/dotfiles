;;; init.el --- thblt's Emacs init script

;;; Commentary:

; Don't trust anything in this file. I just started trying to use Emacs.

;;; Code:

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-initialize)
;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;
(package-install 'ace-window)				; 
(package-install 'auctex)                   ; (La)TeX edition
(package-install 'company)					; Completion
(package-install 'company-auctex)			; Completion provider for AucTeX
(package-install 'company-c-headers)		; Completion provider for C header files
(package-install 'company-ghc)				; Completion provider for Haskell
(package-install 'company-jedi)				; Completion provider for Python
(package-install 'evil)						; Extensible VI Layer
(package-install 'flycheck)					; On-the-fly syntax checking/linting.
(package-install 'flycheck-haskell)			; Haskell provider for Flycheck
(package-install 'flycheck-pyflakes)		; Pyflakes provider for Flycheck
(package-install 'helm)						; Incremental completion and selection narrowing framework.
(package-install 'helm-dash)				; Access Dash docsets through Helm.
(package-install 'magit)					; Git integration
(package-install 'markdown-mode)			; Markdown
(package-install 'projectile)				; Project management
(package-install 'relative-line-numbers)	; À la vim
(package-install 'smart-mode-line)			; Better mode line
(smart-mode-line-enable)
(package-install 'writeroom-mode)			; Distraction-free mode
(package-install 'yasnippet)				; Snippets

;;;;;;;;;;;;;;;; Built-in packages ;;;;;;;;;;;;;;;;
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(load-theme 'atom-one-dark)

(setq-default comment-empty-lines t				; Prefix empty lines too
              inhibit-startup-screen t			; Skip the startup screens
              tab-width 4						; Set tab stops
              use-dialog-box nil				; Always use the minibuffer for prompts
              initial-scratch-message "; Scratch buffer\n\n"
              )

(setq mac-option-modifier 'nil
	  mac-command-modifier 'meta)

;; Look & Feel

(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun startup-echo-area-message ()
  "I'm ready!")

(global-relative-line-numbers-mode)
(projectile-global-mode)

;;; Bindings
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Helm

;;; Helm-dash

(require 'helm-dash)
;(setq helm-dash-browser-func 'eww)

(defun c-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("C")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
	("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
