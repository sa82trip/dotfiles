;;;; minimal-init.el --- A minimal Emacs configuration for learning --

;;; 1. 기본 환경 설정 (가장 먼저 처리하는 것이 좋은 것들)

(setq-default indent-tabs-mode nil)          ; 탭 대신 스페이스 사용
(setq make-backup-files nil)                 ; 백업 파일 생성 안 함
(setq auto-save-default nil)                 ; 자동 저장 파일 생성 안 함
(setq-default cursor-type 'bar)              ; 커서 모양을 바로 변경
(scroll-bar-mode -1)                        ; 스크롤바 끄기
(tool-bar-mode -1)                          ; 툴바 끄기
(menu-bar-mode -1)                          ; 메뉴바 끄기
(setq inhibit-startup-message t)             ; 시작 메시지 끄기

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(setq use-package-always-ensure t) ; :ensure t를 모든 use-package에 기본 적용


;;; 3. 필수 핵심 패키지
;; --------------------------------------------------------------------------
;; 미니버퍼를 강력하게 만들어주는 패키지 (M-x, C-x b 등)
(use-package vertico
:init
(vertico-mode))

;; Vertico에 추가 정보(주석 등)를 달아주는 패키지
(use-package marginalia
:init
(marginalia-mode))

;; --------------------------------------------------------------------------
;; 키 바인딩 길잡이
(use-package which-key
:init
(which-key-mode))

;; --------------------------------------------------------------------------
;; Vim의 vi)와 같은 의미 단위 선택 기능
(use-package expand-region
:bind ("C-=" . er/expand-region))


(use-package company
  :init
  ;; 모든 버퍼에서 company-mode를 활성화합니다.
  (global-company-mode)
  :config
  ;; 0.2초 후에 자동 완성 창이 나타나도록 설정
  (setq company-idle-delay 0.2)
  ;; 최소 2글자를 입력해야 자동 완성이 시작되도록 설정
  (setq company-minimum-prefix-length 2)
  ;; 자동 완성 후보를 가져올 백엔드(Backends) 설정
  (add-to-list 'company-backends 'company-yasnippet))

;; --------------------------------------------------------------------------
;; 실시간 문법 및 스타일 검사
(use-package flycheck
  :init
  ;; 전역적으로 flycheck 모드를 활성화합니다.
  ;; 그러면 Elisp 뿐만 아니라 다른 언어(Python, Java 등)에서도
  ;; 해당 Linter가 설치되어 있으면 자동으로 검사를 수행합니다.
  (global-flycheck-mode))



;; --------------------------------------------------------------------------
;; Lisp 코드의 구조적 편집 (괄호 편집의 제왕)
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  
  ;; :config 대신 :bind를 사용하여 키를 바인딩합니다.
  ;; :map을 통해 paredit-mode-map에만 이 키를 적용하도록 지정합니다.
  ;; 이 방식은 use-package가 알아서 로딩 시점을 조절해 주므로 안전합니다.
  :bind (:map paredit-mode-map
              ("C-c >" . paredit-forward-slurp-sexp)
              ("C-c <" . paredit-forward-barf-sexp)))


;; Elisp 모드에서 eldoc을 활성화하여 함수 정보를 쉽게 확인
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)



;; --------------------------------------------------------------------------
;; 프로젝트 관리
(use-package projectile
  :init
  ;; projectile 모드를 전역적으로 활성화
  (projectile-mode +1)

  ;; projectile의 모든 명령어들을 "C-c p" 라는 prefix 키 아래에 모아줍니다.
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;--------------------------------------------------------------------------
;; 터미널 에뮬레이터
(use-package vterm
  :ensure t)


;; yasnippet 패키지 설정
(use-package yasnippet
  :ensure t        ; yasnippet이 없으면 자동으로 설치
  :init            ; yasnippet이 로드되기 전에 실행될 설정
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ; 사용자 정의 스니펫 디렉토리 (필수)
          ))    ; 기존 스니펫 디렉토리 (패키지 스니펫 등)

  :config         ; yasnippet이 로드된 후 실행될 설정
  (yas-global-mode 1) ; yasnippet 전역 활성화 (모든 버퍼에서 사용 가능)
)


;; yasnippet-snippets 패키지 설정 (Org Mode 스니펫 포함)
(use-package yasnippet-snippets
  :ensure t        ; 패키지가 없으면 자동으로 설치
  :after yasnippet ; yasnippet이 로드된 후에 로드되도록 설정
  :config
  ;; 대부분의 경우 추가 설정 없이 자동으로 스니펫 경로에 추가됩니다.
  (message "yasnippet-snippets loaded with Org Mode snippets!"))


(use-package fido-mode
  ;; :ensure t        ;; 이 줄을 제거합니다!
  :defer t
  :after vertico ;; vertico 로드 후에 fido-mode가 활성화되도록 설정
  :init
  (fido-mode nil)                         ; fido-mode 전역 활성화
  :config
  (message "fido-mode (fuzzy matching for vertico) is enabled after vertico!"))




;;;; End of minimal-init.el~

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fido-mode yasnippet-snippets company-yasnippet yasnippet vterm projectile paredit flycheck company expand-region which-key marginalia vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
