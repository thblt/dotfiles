;;; init.el --- thblt's Emacs init script

;;; Commentary:

;; Don't trust anything in this file.

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

(require 'package)  ; Automated package management, thanks.

(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))                     ; Load package list if absent.

(eval-and-compile
  (package-install 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; ### Packages ### ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Appareance and interaction
(use-package monokai-theme)             ; Theme
(load-theme 'monokai)
(use-package ace-window)                ; Easily switch between windows.
(global-set-key (kbd "M-p") 'ace-window)
;; (use-package diminish)                  ; Don't display some minor modes in modeline
(use-package guru-mode)                 ; Disable common keybindings
(use-package helm)                      ; Incremental completion and selection narrowing framework
(use-package linum-relative            ; Relative line numbers
  :pin melpa) ; 404 somewhere else for some reason
;;(linum-relative-global-mode t)
(setq linum-relative-current-symbol "")      ; Absolute line number on current line
(setq linum-relative-with-helm nil) 
;; (diminish 'linum-relative-mode)
(use-package neotree)                   ; Folders tree sidebar
(global-set-key (kbd "<f2>") 'neotree-toggle)
(use-package smart-mode-line)           ; Better mode line
(smart-mode-line-enable)
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Editing
(use-package anzu)                      ; Highlight search matches, show number/position in mode line
(use-package avy)                       ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(use-package god-mode)                  ; Modal editing
(global-set-key (kbd "<escape>") 'god-local-mode)
(use-package evil)                      ; Extensible VI Layer
(use-package evil-leader)               ; Enable <leader> key 
(use-package evil-surround)             ; A port (?) of tpope's Surround
(use-package evil-nerd-commenter)       ; A port (?) of NerdCommenter
(use-package expand-region)             ; Expand region by semantic units
(use-package highlight-indentation)     ; Show indent level markers
(use-package relative-line-numbers)     ; À la vim
(use-package smartparens)               ; Be smart with parentheses
(use-package writeroom-mode)            ; Distraction-free mode
(use-package yasnippet)                 ; Snippets

;; Versioning and history
(use-package git-timemachine)           ; Traverse a file's git history
(use-package magit)                     ; Git porcelain integration

;; Project management
(use-package projectile)                ; Project management
(projectile-global-mode)
;(diminish 'projectile-mode)
(setq rm-blacklist (quote (" Projectile" " Undo-Tree")))

;; General programming
(use-package company)                   ; Completion framework
;;(eval-after-load "company" '(diminish 'company-mode))
(use-package flycheck)                  ; On the fly checking/linting
;; (eval-after-load "flycheck" '(diminish 'flycheck-mode)) 
(use-package helm-dash)                 ; Access Dash docsets through Helm.
(global-set-key (kbd "<f1>") 'helm-dash-at-point) 

;; === Syntaxes ===
;; C/C++
(use-package clang-format)              ; Interface to clang-format
(use-package cpputils-cmake)            ; Automatic configuration for Flycheck/Company/etc for CMake projects
(use-package company-c-headers)         ; Completion provider for C header files

;; CSS/SCSS/LESS
(use-package scss-mode)                 ; (S)CSS
(use-package less-css-mode)             ; LESS

;; Haskell
(use-package company-ghc)               ; Completion provider for Haskell
(use-package flycheck-haskell)          ; Haskell provider for Flycheck
(use-package helm-hoogle)               ; Search Hoogle 

;; HTML (template)
(use-package haml-mode)                 ; HAML templates
(use-package web-mode)                  ; HTML and HTML templates

;; Markdown
(use-package markdown-mode)             ; Markdown major mode

;; TeX
(use-package tex
  :ensure auctex
  :init (add-hook 'LaTeX-mode-hook (progn
									 'turn-on-flyspell
									 'toggle-word-wrap
									 'TeX-fold-mode
									 )
				  )
  :config (setq TeX-save-query nil)
  )                                     ; (La)TeX edition
;;(use-package company-auctex)          ; Completion provider for AucTeX

;; Python
(use-package company-jedi)              ; Completion provider for Python
(use-package flycheck-pyflakes)         ; Pyflakes provider for Flycheck

;; YAML
(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; General look and feel ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (string= system-type 'darwin)
  (menu-bar-mode -1)                         ; There's no gain in hiding menu bar on OSX.
  )
(when window-system (tool-bar-mode -1) (scroll-bar-mode -1))

(setq-default comment-empty-lines t          ; Prefix empty lines too
    inhibit-startup-screen t                 ; Skip the startup screens
    tab-width 4								 ; Set tab stops
    use-dialog-box nil						 ; Always use the minibuffer for prompts
    initial-scratch-message "; Scratch buffer\n\n"
    )

;;; OSX keyboard and stuff
(when (string= system-type 'darwin) 
  (setq mac-option-modifier 'nil
        mac-command-modifier 'meta)
  
  (global-set-key (kbd "<help>") 'overwrite-mode)                    ; Fix weird Apple keymap.
  )

;;; OSX graphical Emacs fixes
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
	:init (exec-path-from-shell-initialize) ; Load PATH from shell in Cocoa
	)
  )

;;(global-linum-mode t)
(column-number-mode t)
(line-number-mode nil)

;; Look & Feel

(defun startup-echo-area-message ()
  "I'm ready!")                                                      ; Because SpongeBob.

;;; Bindings

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Let's keep the mess out of my filesystem

(server-start)

(provide 'init)
;;; init.el ends here
