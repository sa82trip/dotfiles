;;;; minimal-init.el --- A minimal Emacs configuration for learning --

;;; 1. Basic Environment Settings (Best to handle these first)

(setq-default indent-tabs-mode nil)          ; Use spaces instead of tabs
(setq make-backup-files nil)                 ; Do not create backup files
(setq auto-save-default nil)                 ; Do not create auto-save files
(setq-default cursor-type 'bar)              ; Change cursor shape to a bar
(scroll-bar-mode -1)                        ; Turn off the scroll bar
(tool-bar-mode -1)                          ; Turn off the tool bar
(menu-bar-mode t)                           ; Turn on the menu bar
(setq inhibit-startup-message t)             ; Turn off the startup message

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t) ; Apply :ensure t to all use-package declarations by default

;;; 3. Core Essential Packages
;; --------------------------------------------------------------------------
;; A package that enhances the minibuffer (M-x, C-x b, etc.)
(use-package vertico
:init
(vertico-mode))

;; A package that adds additional information (annotations, etc.) to Vertico
(use-package marginalia
:init
(marginalia-mode))

;; --------------------------------------------------------------------------
;; Key binding helper
(use-package which-key
:init
(which-key-mode))

;; --------------------------------------------------------------------------
;; Semantic selection feature like vi) in Vim
(use-package expand-region
:bind ("C-=" . er/expand-region))


(use-package company
  :init
  ;; Activate company-mode in all buffers.
  (global-company-mode)
  :config
  ;; Set the auto-completion window to appear after 0.2 seconds
  (setq company-idle-delay 0.2)
  ;; Start auto-completion after typing at least 2 characters
  (setq company-minimum-prefix-length 2)
  ;; Set the backends to fetch auto-completion candidates
  (add-to-list 'company-backends 'company-yasnippet))



;; --------------------------------------------------------------------------
;; Real-time syntax and style checking
(use-package flycheck
  :init
  ;; Activate flycheck-mode globally.
  ;; This will automatically perform checks in other languages (Python, Java, etc.)
  ;; if the corresponding linter is installed.
  (global-flycheck-mode))



;; --------------------------------------------------------------------------
;; Structural editing of Lisp code (The king of parenthesis editing)
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  
  ;; Use :bind instead of :config to bind keys.
  ;; Use :map to apply this key only to paredit-mode-map.
  ;; This method is safe because use-package handles the loading time.
  :bind (:map paredit-mode-map
              ("C-c >" . paredit-forward-slurp-sexp)
              ("C-c <" . paredit-forward-barf-sexp)))


;; Activate eldoc in Elisp mode to easily check function information
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)



;; --------------------------------------------------------------------------
;; Project Management
(use-package projectile
  :init
  ;; Activate projectile-mode globally
  (projectile-mode +1)
  :diminish projectile-mode ; Keep the mode line clean
  ;; Group all projectile commands under the "C-c p" prefix key.
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;--------------------------------------------------------------------------
;; Terminal Emulator
(use-package vterm
  :ensure t)


;; yasnippet package configuration
(use-package yasnippet
  :ensure t        ; Automatically install yasnippet if not present
  :init            ; Settings to be executed before yasnippet is loaded
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ; Custom snippet directory (required)
          ))    ; Existing snippet directory (package snippets, etc.)

  :config         ; Settings to be executed after yasnippet is loaded
  (yas-global-mode 1) ; Enable yasnippet globally (available in all buffers)
)


;; yasnippet-snippets package configuration (including Org Mode snippets)
(use-package yasnippet-snippets
  :ensure t        ; Automatically install the package if not present
  :after yasnippet ; Load after yasnippet is loaded
  :config
  ;; In most cases, it is automatically added to the snippet path without additional configuration.
  (message "yasnippet-snippets loaded with Org Mode snippets!"))


(use-package fido-mode
  :ensure nil        ;; Remove this line!
  :defer t
  :after vertico ;; Activate fido-mode after vertico is loaded
  :init
  (fido-mode nil)                         ; Activate fido-mode globally
  :config
  (message "fido-mode (fuzzy matching for vertico) is enabled after vertico!"))

(use-package seoul256-theme
  :config
  (setq seoul256-background 252)
  (load-theme 'seoul256 t))


(use-package exec-path-from-shell)
(use-package ripgrep)


;; Display related settings
(set-face-attribute 'mode-line nil :height 150)
(set-face-attribute 'mode-line-inactive nil :height 100)


;; org mode related settings
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●" "◉" "○" "◆" "▶" "▷" "▸")))

;; Make help look better
(use-package helpful
  :ensure t)


(setq bookmark-save-flag 1)


;;;; End of minimal-init.el~

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helpful org-bullets ripgrep exec-path-from-shell seoul256-theme fido-mode yasnippet-snippets company-yasnippet yasnippet vterm projectile paredit flycheck company expand-region which-key marginalia vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )