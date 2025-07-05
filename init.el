;;(load-theme 'modus-vivendi t)
(global-display-line-numbers-mode 1)


;;(global-auto-revert-mode 1)
;;(setq auto-save-default t)
;;(setq auto-save-interval 300) 


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) 

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package ivy
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package projectile
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key general evil-collection evil cider doom-themes ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'cider)
;; 예시: REPL 시작 시 현재 파일의 네임스페이스로 자동 변경
(setq cider-repl-auto-to-ns t)


(let ((path-to-lein "/opt/homebrew/bin/")) ; 여기에 실제 lein 실행 파일 경로를 입력하세요.
  (unless (member path-to-lein exec-path)
    (setq exec-path (append exec-path (list path-to-lein)))))


(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))


(use-package general
  :ensure t)

(use-package general
  :config
  (general-create-definer leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal insert emacs))  ;; 모든 상태에서 적용[5]

  (leader-def
    ":" 'counsel-M-x))  ;; SPC : → M-x 실행
(setq which-key-idle-delay 0.5)  ;; 팝업 표시 대기 시간
(setq which-key-show-remaining-keys t)  ;; 남은 키 표시[6]

