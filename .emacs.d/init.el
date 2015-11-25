;;; init.el --- thblt's Emacs init script

;;; Commentary:

; Don't trust anything in this file. I just started trying to use Emacs.

;;; Code:

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; Let customize put its mess elsewhere
(setq custom-file "~/.emacs.d/_customize.el")

(load-user-file "_customize.el")

;; Automated package management, thanks.
(require 'package)

(setq package-archives '(("gnu"			. "https://elpa.gnu.org/packages/")
                         ("marmalade"	. "https://marmalade-repo.org/packages/")
                         ("melpa"		. "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; === Packages === ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-install 'ace-window)				; 
(package-install 'anzu)
(package-install 'avy)
(package-install 'atom-one-dark-theme)		; Theme
(package-install 'auctex)                   ; (La)TeX edition
(package-install 'company)					; Completion
(package-install 'company-auctex)			; Completion provider for AucTeX
(package-install 'company-c-headers)		; Completion provider for C header files
(package-install 'company-ghc)				; Completion provider for Haskell
(package-install 'company-jedi)				; Completion provider for Python
(package-install 'cpputils-cmake)           ; Automatic configuration for Flycheck/Company/etc for CMake projects
(package-install 'diminish)                 ; Don't display some minor modes in modeline
(package-install 'evil)						; Extensible VI Layer
(package-install 'expand-region)            ; Expand region by semantic units
(package-install 'flycheck)					; On-the-fly syntax checking/linting.
(package-install 'flycheck-haskell)			; Haskell provider for Flycheck
(package-install 'flycheck-pyflakes)		; Pyflakes provider for Flycheck
(package-install 'git-timemachine)          ;
(package-install 'god-mode)                 ; 
(package-install 'guru-mode)                ; Disable common keybindings
(package-install 'helm)						; Incremental completion and selection narrowing framework.
(package-install 'helm-dash)				; Access Dash docsets through Helm.
(package-install 'magit)					; Git integration
(package-install 'markdown-mode)			; Markdown
(package-install 'projectile)				; Project management
(package-install 'relative-line-numbers)	; À la vim
(package-install 'smart-mode-line)			; Better mode line
(package-install 'smartparens)              ; Be smart with parentheses
(package-install 'writeroom-mode)			; Distraction-free mode
(package-install 'yasnippet)				; Snippets

;;;;;;;;;;;;;;;; Built-in packages ;;;;;;;;;;;;;;;;
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(load-theme 'atom-one-dark)
(smart-mode-line-enable)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default comment-empty-lines t				; Prefix empty lines too
              inhibit-startup-screen t			; Skip the startup screens
              tab-width 4						; Set tab stops
              use-dialog-box nil				; Always use the minibuffer for prompts
              initial-scratch-message "; Scratch buffer\n\n"
              )

;;; OSX keyboard
(setq mac-option-modifier 'nil
	  mac-command-modifier 'meta)

;;; OSX keyboard *on Linux*

;; Look & Feel

(defun startup-echo-area-message ()
  "I'm ready!")

(projectile-global-mode)

;;; Bindings
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Helm

;;; Helm-dash

(require 'helm-dash)
;(setq helm-dash-browser-func 'eww)

(provide 'init)
;;; init.el ends here
