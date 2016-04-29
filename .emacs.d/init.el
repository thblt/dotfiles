;; init.el --- thblt's Emacs init script.

;;; Commentary:

;; Don't trust anything in this file.

;;; Code:

;; Identify computer
(setq thblt/base-font-size 090)
(when (string-prefix-p system-name "rudiger")
  (setq thblt/base-font-size 110)
  )

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")
        )
  )

(defun load-user-file (file)
  (interactive "fExecute: ")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir))
  )

;; ╔═╗┌─┐┌─┐┬┌─┌─┐┌─┐┌─┐  ╔╦╗┌─┐┌┐┌┌─┐┌─┐┌─┐┌┬┐┌─┐┌┐┌┌┬┐
;; ╠═╝├─┤│  ├┴┐├─┤│ ┬├┤   ║║║├─┤│││├─┤│ ┬├┤ │││├┤ │││ │
;; ╩  ┴ ┴└─┘┴ ┴┴ ┴└─┘└─┘  ╩ ╩┴ ┴┘└┘┴ ┴└─┘└─┘┴ ┴└─┘┘└┘ ┴

(require 'package)

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

;; ╔═╗┌─┐┌┐┌┌─┐┬─┐┌─┐┬
;; ║ ╦├┤ │││├┤ ├┬┘├─┤│
;; ╚═╝└─┘┘└┘└─┘┴└─┴ ┴┴─┘

(setq-default
 ;; Modeline
 column-number-mode t       ; Column number in modeline
 line-number-mode t         ; Line - - -

 ;; Indentation and general editing
 tab-width 4                ; Set tab stops
 indent-tabs-mode nil       ; Default to spaces for indent (smart tabs
                            ; on some syntaxes, see below)
 comment-empty-lines t      ; 
 reb-re-syntax 'string      ; String syntax for re-builder

 ;; Stuff for programming
 compile-command "wmake"    ; A small script which invokes the first
                            ; build system it can find instructions
                            ; for (in .dotfiles/bin)
 
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "setsid"
 browse-url-generic-args '("xdg-open")
 ;; setsid xdg-open prevents emacs from killing xdg-open before it
 ;; actually opened anything.  see
 ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
 
 ;; General interface improvements
 vc-follow-symlinks t       ; Always follow symlinks to
                            ; version-controlled files.
 use-dialog-box nil         ; Always use the minibuffer for prompts
 inhibit-startup-screen t   ; Skip the startup screens

 initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n"
 )

;;; === Sanity ===
(fset 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

;; Autosave and backups in /tmp/ 
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Let Customize put its mess elsewhere
(setq custom-file (concat user-init-dir "_customize.el"))
(load custom-file)

;; === Look and feel ===

(setq default-frame-alist '((font . "DejaVu Sans Mono for Powerline")))
(set-face-attribute 'default t :height thblt/base-font-size)



(unless (string= 'system-type 'darwin) (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)
            )
          ) ; Save everything on losing focus @TODO Make silent:
            ; disable "(No files need saving)", autocreate directories
            ; when needed.

;;; OSX-specific configuration
(when (string= system-type 'darwin) 
  (setq mac-option-modifier 'nil
        mac-command-modifier 'meta)
  
  (global-set-key (kbd "<help>") 'overwrite-mode)                  ; Fix weird Apple keymap.on full-size kbs.
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e") ; Fix load-path for mu4e (not sure this is still needed)
  )

;;; OSX Cocoa path fix
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize) ; Load PATH from shell in Cocoa
    )
  )

(use-package tao-theme
  :defer t
  )

(use-package spacemacs-theme
  :defer t)

(use-package alect-themes
  :defer t)

(load-theme 'alect-dark)
;; (use-package ace-window                 ; Easily switch between windows.
;;   :init (setq aw-dispatch-always t)
;;   :config (set-face-attribute 'aw-leading-char-face nil
;;                               :height 240
;;                               :background "#ccff33"
;;                               :foreground "black"
;;                               )
;; 	:bind ("M-p" . ace-window)
;; 	)
(use-package helm)                      ; Incremental completion and selection narrowing framework
(use-package helm-ag)                   ; The silver searcher
(use-package nlinum                     ; More efficient line numbering, especially on large file with huge foldings (eg org)
  :config (nlinum-mode)
  )
(use-package linum-relative             ; Relative line numbers
;;  :init (linum-relative-global-mode)
  :config (setq linum-relative-current-symbol ""
                linum-relative-with-helm nil)
  )

(use-package neotree                    ; FS sidebar à la NERDTree
  :bind ("<f2>" . neotree-toggle)
  )

(use-package diminish)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  )

(use-package windmove
  :init (windmove-default-keybindings)
  )

;; Editing
(use-package anzu)                      ; Show matches count/current match # in mode line
(use-package avy                        ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
  :bind (("C-:" . avy-goto-char-timer)
;;         ("C-M-:" . avy-goto-char-timer)
         ("C-=" . avy-goto-line)
         )
  )

(use-package undo-tree
	     :config (diminish 'undo-tree-mode)
	     )

(use-package evil                       ; Extensible VI Layer
  :config (progn
			(setq evil-insert-state-cursor '(bar))
			;; Ctrl belongs to Emacs's realm: use normal Emacs bindings.
			;; @TODO Should be done using :bind or something, but I haven't
			;; yet found how. (Seeing use-package docs, it seems it can't be
			;; done. See color-moccur example in README.md)
			(define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
			(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
			(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
			; In insert mode, `evil-end-of-line' puts the cursor *before*
			; the last character, which is useless.
			(define-key evil-insert-state-map "\C-e" 'end-of-line) 
			(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			(define-key evil-insert-state-map "\C-k" 'kill-line)
			(define-key evil-insert-state-map "\C-n" 'evil-next-line)
			(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
			(define-key evil-insert-state-map "\C-w" 'evil-delete)
			(define-key evil-insert-state-map "\C-y" 'yank)
			(define-key evil-motion-state-map "\C-e" 'end-of-line)
			(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
			(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
			(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
			(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
			(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
			(define-key evil-normal-state-map "\C-k" 'kill-line)
			(define-key evil-normal-state-map "\C-n" 'evil-next-line)
			(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
			(define-key evil-normal-state-map "\C-w" 'evil-delete)
			(define-key evil-normal-state-map "\C-y" 'yank)
			(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
			(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
			(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
			(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
			(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
			(define-key evil-visual-state-map "\C-k" 'kill-line)
			(define-key evil-visual-state-map "\C-n" 'evil-next-line)
			(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
			(define-key evil-visual-state-map "\C-w" 'evil-delete)
			(define-key evil-visual-state-map "\C-y" 'yank)
			)
	;; :init (progn
	;; 				;; Restrict Evil to text-editing modes.
    ;;       ;; FIXME This won't work in Fundamental mode.
    ;;       (add-hook 'conf-mode-hook 'evil-local-mode)
	;; 				(add-hook 'text-mode-hook 'evil-local-mode)
	;; 				(add-hook 'prog-mode-hook 'evil-local-mode)
	;; 				)
  )

;; (use-package evil-leader)            ; Enable <leader> key 
(use-package evil-surround              ; A port of tpope's Surround
  :config (global-evil-surround-mode t)
  )
(use-package evil-nerd-commenter       ; A port of NerdCommenter
 ;; :config (evilnc-default-hotkeys)
  )
(use-package expand-region)             ; Expand region by semantic units
(use-package highlight-indentation)     ; Show indent level markers
(use-package rainbow-delimiters)        ; Colorize parentheses etc by depth.
(use-package smartparens-confg          ; Be smart with parentheses
  :ensure smartparens
  :init (progn
          (show-smartparens-global-mode t)
          )
  )
(use-package aggressive-indent)
(use-package smart-tabs-mode
  :init (add-hook 'c-mode-common-hook (lambda ()
                                    (smart-tabs-mode-enable)
                                    )
                  )
;;  :config (smart-tabs-insinuate 'c 'c++ 'python 'javascript)
  )

(use-package writeroom-mode)            ; Distraction-free mode
(use-package yasnippet                  ; Snippets
  :init (yas-global-mode)
  :config (diminish 'yas-minor-mode)
  )

;; Versioning and history
(use-package git-timemachine)           ; Traverse a file's git history
(use-package magit
  :defer t)                             ; Git integration

;; Project management
(use-package projectile                 ; Project management
  :init (projectile-global-mode)
  :config (diminish 'projectile-mode)
  )

;; General programming

(bind-key (kbd "<f5>") (lambda ()
                         (interactive)
                         (save-some-buffers t)
                         (recompile)
                         )
          )

(use-package company                    ; Completion framework
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config (progn
            (diminish 'company-mode)
            )
  )
(use-package flycheck                   ; On the fly checking/linting
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config (progn
            (diminish 'flycheck-mode)
            )
  )

(use-package helm-dash                  ; Access Dash docsets through Helm.
  :bind ("<f1>" . helm-dash-at-point)
  )

(add-hook 'python-mode-hook (lambda ()
                              (setq-local helm-dash-docsets '("Python 2" "Python 3"))
                              )
          )

;; ╔═╗┬ ┬┌┐┌┌┬┐┌─┐─┐ ┬┌─┐┌─┐
;; ╚═╗└┬┘│││ │ ├─┤┌┴┬┘├┤ └─┐
;; ╚═╝ ┴ ┘└┘ ┴ ┴ ┴┴ └─└─┘└─┘

;; BibTeX
(use-package ebib                       ; BibTex editing *app* 
  :config (setq ebib-bibtex-dialect 'biblatex)
  )

;; C/C++
(use-package clang-format)              ; Interface to clang-format
(use-package company-c-headers)         ; Completion provider for C header file
(use-package cpputils-cmake)            ; Automatic configuration for Flycheck/Company/etc for CMake projects
(use-package irony)
(use-package flycheck-irony)
(use-package company-irony)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; From irony docs
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (irony-mode t)
            (irony-cdb--autodetect-compile-options)
            (setq-local helm-dash-docsets '("C" "C++" "Qt"))
            )
          )

;; CMake
(use-package cmake-mode)

;; CSS/SCSS/LESS
(use-package scss-mode)                 ; (S)CSS
(use-package less-css-mode)             ; LESS

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local custom--hooksets '("Emacs Lisp"))
            )
          )

;; Haskell
(use-package haskell-mode)
(use-package company-ghc)               ; Completion provider for Haskell
(use-package flycheck-haskell)          ; Haskell provider for Flycheck
(use-package helm-hoogle)               ; Search Hoogle 

(add-hook 'haskell-mode-hook 
          (lambda ()
            (setq-local helm-dash-docsets '("Haskell"))
            )
          )

;; HTML (template)
(use-package haml-mode)                 ; HAML templates
(use-package web-mode)                  ; HTML and HTML templates

(add-hook 'html-mode-hook
          (lambda ()      
            (setq-local helm-dash-docsets '("HTML"))
            )
          )

;; Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("JavaScript"))
            )
          )

;; Lua
(use-package lua-mode)

;; markdown
(use-package markdown-mode)             ; Markdown major mode

;; Python
(use-package company-jedi)              ; Completion provider for Python
(use-package flycheck-pyflakes)         ; Pyflakes provider for Flycheck

;; TeX
(use-package tex-site
  :ensure auctex
  :init (add-hook 'LaTeX-mode-hook (progn
                                     'turn-on-flyspell
                                     'toggle-word-wrap
                                     'TeX-fold-mode
                                     )
                  ) 
  :config (setq TeX-save-query nil)     ; Autosave
  )                                     ; (La)TeX edition
(use-package company-auctex)            ; Completion provider for AucTeX

;; XML
(use-package emmet-mode)

;; YAML
(use-package yaml-mode)

;;; === Decoration === 

(defun startup-echo-area-message ()
  "I'm ready!") ; Because SpongeBob.

;;; Bindings

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Other configuration modules

(load-user-file "init-email.el")
(load-user-file "init-mmm-markdown.el")

(load "server")
(unless (server-running-p) ;; This should not happen.
  (server-start)
  )

(provide 'init)
;;; init.el ends here
