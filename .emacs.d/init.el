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

(setq package-archives '(("gnu"      . "https://elpa.gnu.org/packages/")
                         ("marmalade"  . "https://marmalade-repo.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))                     ; Load package list if absent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; ### Packages ### ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Appareance and interaction
(package-install 'monokai-theme)             ; Theme
(load-theme 'monokai)
(package-install 'ace-window)                ; Easily switch between windows.
(global-set-key (kbd "M-p") 'ace-window)
(package-install 'diminish)                  ; Don't display some minor modes in modeline
(package-install 'guru-mode)                 ; Disable common keybindings
(package-install 'helm)                      ; Incremental completion and selection narrowing framework.
(package-install 'neotree)                   ; Folders tree sidebar
(global-set-key (kbd "<f2>") 'neotree-toggle)
(package-install 'smart-mode-line)           ; Better mode line
(smart-mode-line-enable)
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Editing
(package-install 'anzu)                      ; Highlight search matches, show number/position in mode line
(package-install 'avy)                       ; Jump, move and copy everywhere (similar to Vim-EasyMotion)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(package-install 'god-mode)                  ; Modal editing
(global-set-key (kbd "<escape>") 'god-local-mode)
(package-install 'evil)                      ; Extensible VI Layer
(package-install 'expand-region)             ; Expand region by semantic units
(package-install 'highlight-indentation)     ; Show indent level markers
(package-install 'relative-line-numbers)     ; À la vim
(package-install 'smartparens)               ; Be smart with parentheses
(package-install 'writeroom-mode)            ; Distraction-free mode
(package-install 'yasnippet)                 ; Snippets

;; Versioning and history
(package-install 'git-timemachine)           ; Traverse a file's git history
(package-install 'magit)                     ; Git porcelain integration

;; Project management
(package-install 'projectile)                ; Project management
(projectile-global-mode)
(diminish 'projectile-mode)

;; General programming
(package-install 'company)                   ; Completion framework
(eval-after-load "company" '(diminish 'company-mode))
(package-install 'flycheck)                  ; On the fly checking/linting
(eval-after-load "flycheck" '(diminish 'flycheck-mode)) 
(package-install 'helm-dash)                 ; Access Dash docsets through Helm.
(global-set-key (kbd "<f1>") 'helm-dash-at-point) 

;; === Syntaxes ===
;; C/C++
(package-install 'clang-format)              ; Interface to clang-format
(package-install 'cpputils-cmake)            ; Automatic configuration for Flycheck/Company/etc for CMake projects
(package-install 'company-c-headers)         ; Completion provider for C header files

;; Haskell
(package-install 'company-ghc)               ; Completion provider for Haskell
(package-install 'flycheck-haskell)          ; Haskell provider for Flycheck
(package-install 'helm-hoogle)               ; Search Hoogle 

;; Markdown
(package-install 'markdown-mode)             ; Markdown major mode

;; TeX
(package-install 'auctex)                    ; (La)TeX edition
(package-install 'company-auctex)            ; Completion provider for AucTeX

;; Python
(package-install 'company-jedi)              ; Completion provider for Python
(package-install 'flycheck-pyflakes)         ; Pyflakes provider for Flycheck

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
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:~/bin"))   ; Fix path on OSX
  )

;; Look & Feel

(defun startup-echo-area-message ()
  "I'm ready!")                                                      ; Emacs == SpongeBob

;;; Bindings

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Let's keep the mess out of my filesystem

(server-start)

(provide 'init)
;;; init.el ends here
