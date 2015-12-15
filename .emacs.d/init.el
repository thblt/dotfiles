;; init.el --- thblt's Emacs init script.

;;; Commentary:

;; Don't trust anything in this file.

;;; Code:

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

;;; === One wants one's filesystem clean ===

; Autosave and backups in /tmp/ 
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;; Let customize put its mess elsewhere
(setq custom-file (concat user-init-dir "customize_autogen.el"))
(load custom-file)

;;; === One needs some extra packages === 

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

;; ╔═╗┌─┐┌┐┌┌─┐┬─┐┌─┐┬
;; ║ ╦├┤ │││├┤ ├┬┘├─┤│
;; ╚═╝└─┘┘└┘└─┘┴└─┴ ┴┴─┘

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

(add-hook 'focus-out-hook
		  (lambda ()
			(save-some-buffers t)
			)
		  ) ; Save everything on losing focus.

;;; OSX-specific configuration
(when (string= system-type 'darwin) 
  (setq mac-option-modifier 'nil
        mac-command-modifier 'meta)
  
  (global-set-key (kbd "<help>") 'overwrite-mode)                    ; Fix weird Apple keymap.
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")        ; Fix load-path for mu4e
  )

;;; OSX Cocoa path fix
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
	:init (exec-path-from-shell-initialize) ; Load PATH from shell in Cocoa
	)
  )

;; Appareance and interaction
(setq column-number-mode t
	  line-number-mode nil
	  )

(use-package monokai-theme             
  :init (load-theme 'monokai))          ; Theme
(use-package ace-window)                ; Easily switch between windows.
(global-set-key (kbd "M-p") 'ace-window)
(use-package helm)                      ; Incremental completion and selection narrowing framework
(use-package helm-ag)
(use-package linum-relative             ; Relative line numbers
  :pin melpa                            ; 404 somewhere else for some reason
  :init (linum-relative-global-mode)
  :config (setq linum-relative-current-symbol ""
				linum-relative-with-helm nil)
  ) 

(use-package neotree                    ; FS sidebar à la NERDTree
  :bind ("<f2>" . neotree-toggle)
  )

(use-package smart-mode-line         ; Better mode line
  :config (sml/setup) 
  :init
  (setq rm-blacklist
		(format "^ \\(%s\\)$"
				(mapconcat #'identity
						   '("\\$"      ; Rich minority itself
							 "company"
							 "FlyC.*"
							 "LR"      ; Limum-Relative
							 "Projectile.*"
							 "SP.*"    ; Smartparens
							 "Undo-Tree"
							 "yas")    ; Yasnippet
						   "\\|"))
		)
  )

(use-package windmove
  :init (windmove-default-keybindings)
  )

;; Editing
(use-package anzu)                      ; Show matches count/current match # in mode line
(use-package avy                        ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
  :bind (("C-:" . avy-goto-char-2)
		 ("C-=" . avy-goto-line)
		 )
  )

(use-package evil                       ; Extensible VI Layer
  :config (progn
			(evil-mode)
			(setq evil-insert-state-cursor '(bar))

			;; Ctrl-something is Emacs's realm: use normal Emacs bindings.
			;; @TODO Should be done using :bind or something, but I haven't yet found how.		
			(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
			(define-key evil-insert-state-map "\C-e" 'end-of-line)
			(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
			(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
			(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
			(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
			(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
			(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
			(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
			(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
			(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
			(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
			(define-key evil-normal-state-map "\C-n" 'evil-next-line)
			(define-key evil-insert-state-map "\C-n" 'evil-next-line)
			(define-key evil-visual-state-map "\C-n" 'evil-next-line)
			(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
			(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
			(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
			(define-key evil-normal-state-map "\C-w" 'evil-delete)
			(define-key evil-insert-state-map "\C-w" 'evil-delete)
			(define-key evil-visual-state-map "\C-w" 'evil-delete)
			(define-key evil-normal-state-map "\C-y" 'yank)
			(define-key evil-insert-state-map "\C-y" 'yank)
			(define-key evil-visual-state-map "\C-y" 'yank)
			(define-key evil-normal-state-map "\C-k" 'kill-line)
			(define-key evil-insert-state-map "\C-k" 'kill-line)
			(define-key evil-visual-state-map "\C-k" 'kill-line)
			(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
			(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
			(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
			)
  )
;; (use-package evil-leader)            ; Enable <leader> key 
(use-package evil-surround              ; A port of tpope's Surround
  :config (global-evil-surround-mode t)
  )
(use-package evil-nerd-commenter       ; A port of NerdCommenter
  :config (evilnc-default-hotkeys)
  )
(use-package expand-region)             ; Expand region by semantic units
(use-package highlight-indentation)     ; Show indent level markers
(use-package smartparens-confg          ; Be smart with parentheses
  :ensure smartparens
  :init (progn
		  (show-smartparens-global-mode t)
		  )
  )

(use-package writeroom-mode)            ; Distraction-free mode
(use-package yasnippet                  ; Snippets
  :init (yas-global-mode)
  )

;; Versioning and history
(use-package git-timemachine)           ; Traverse a file's git history
(use-package magit)                     ; Git porcelain integration

;; Project management
(use-package projectile                 ; Project management
  :init (projectile-global-mode)
  )
;; General programming
(use-package company                    ; Completion framework
  :init (add-hook 'prog-mode-hook 'company-mode)
)
(use-package flycheck                   ; On the fly checking/linting
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
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
(use-package cpputils-cmake)            ; Automatic configuration for Flycheck/Company/etc for CMake projects
(use-package company-c-headers)         ; Completion provider for C header files

(add-hook 'c-mode-common-hook
		  (lambda ()
			(setq-local helm-dash-docsets '("C" "C++" "Qt"))
			)
		  )

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

;; Markdown
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

;; YAML
(use-package yaml-mode)

;;; === Email. A new, modern way of getting spam ===

(use-package mu4e
  :ensure nil ; Comes with mu, not on a Emacs package repo
  :config (progn
			(add-to-list 'load-path "/usr/share/emacs/site-lisp/") ; OSX and Debian both use this.
			)
  :init (progn
		  (setq mu4e-maildir "~/.Mail/")
		  
		  (setq mu4e-sent-folder "/P1/sent-mail"
				mu4e-drafts-folder "/P1/Drafts"
				mu4e-trash-folder "/P1/Trash"
				user-mail-address "thibault.polge@univ-paris1.fr"
				smtpmail-default-smtp-server "smtp.univ-paris1.fr"
				smtpmail-local-domain "univ-paris1.fr"
				smtpmail-smtp-server "smtp.account1.tld"
				smtpmail-stream-type 'starttls
				smtpmail-smtp-service 25)
		  
		  (setq mu4e-bookmarks `( ("m:/P1/INBOX OR m:/Namo/INBOX"       "Global inbox"            ?i)
								  ("f:unread"                           "Unread messages"         ?v)
								  ("flag:flagged"                       "Flagged"                 ?f)
								  ) )
		  )
  )

;;; === Feeds

(use-package elfeed
  :config (progn
			(setq elfeed-feeds '(
								 ("https://xkcd.com/atom.xml")
								 ("http://www.maitre-eolas.fr/feed/atom")
								 ("http://binaire.blog.lemonde.fr/feed/")
								 )
				  )
			)
  :init (elfeed-goodies/setup)
  )

(use-package elfeed-goodies)


;;; === Decoration === 

(defun startup-echo-area-message ()
  "I'm ready!") ; Because SpongeBob.

;;; Bindings

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(server-start)

(provide 'init)
;;; init.el ends here
