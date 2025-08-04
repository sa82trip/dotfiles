(setq debug-on-error t)
;;;; minimal-init.el --- 학습을 위한 최소한의 Emacs 설정 ---

;; 이 파일은 학습과 쉬운 이해를 위해 설계된 최소한의 Emacs 설정입니다.
;; 다양한 설정과 패키지 구성을 탐색하고 이해하는 데 도움이 되도록
;; 논리적인 섹션으로 구성되어 있습니다.

;;;; 0. 속도 관련
;; Faster startup: adjusting the frequency of garbage collection
;; REF: https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(setq display-time-format "%Y-%m-%d %a %H:%M")
(display-time-mode 1)
(setq org-src-preserve-indentation t)

(setq system-time-locale "en_US.UTF-8")

;;;; 1. 기본 Emacs 환경 설정

;; 이 설정들은 들여쓰기, 백업 동작, 커서 모양, UI 요소 등
;; Emacs 환경의 기본적인 측면을 구성합니다.

;; 들여쓰기에 탭 대신 공백을 사용합니다. 이는 다른 편집기에서도
;; 일관된 들여쓰기를 보장하기 위한 일반적인 코딩 스타일 선호 사항입니다.
(setq-default indent-tabs-mode nil)

;; Emacs가 백업 파일(예: file.txt~)을 생성하지 않도록 합니다.
;; 버전 관리를 사용한다고 가정할 때 디렉토리를 더 깔끔하게 유지합니다.
(setq make-backup-files nil)

;; Emacs가 자동 저장 파일(예: #file.txt#)을 생성하지 않도록 합니다.
;; 백업 파일과 유사하게, 이는 불필요한 파일을 줄입니다.
(setq auto-save-default nil)

;; 기본 커서 모양을 수직 막대로 설정합니다.
;; 다른 일반적인 옵션으로는 'box, 'hollow, 또는 'nil (커서 없음)이 있습니다.
(setq-default cursor-type 'box)

;; 그래픽 스크롤 바를 끕니다.
;; 많은 사용자가 키보드 단축키나 다른 방법을 사용하여 탐색하는 것을 선호합니다.
(scroll-bar-mode -1)

;; 그래픽 도구 모음을 끕니다.
;; 이는 화면 공간을 확보하고 키보드 중심의 사용을 장려합니다.
(tool-bar-mode -1)

;; 그래픽 메뉴 바를 켭니다.
;; 메뉴 바는 Emacs 명령 및 기능에 쉽게 접근할 수 있도록 합니다.
(menu-bar-mode t)

;; Emacs 시작 메시지(스플래시 화면)를 억제합니다.
;; 이는 더 깔끔하고 빠른 시작 경험을 제공합니다.
(setq inhibit-startup-message t)

;; F8 키를 `execute-extended-command` (M-x)에 바인딩합니다.
;; 이를 통해 명령 이름을 입력하여 Emacs 명령에 빠르게 접근할 수 있습니다.
(global-set-key (kbd "<f8>") 'execute-extended-command)

;; Emacs 종료 시 북마크를 자동으로 저장합니다.
;; 이는 저장된 위치가 세션 간에 지속되도록 보장합니다.
(setq bookmark-save-flag 1)

;; To make ibuffer default
(defalias 'list-buffers 'ibuffer) ; make ibuffer default


;;;; 2. 패키지 관리 설정

;; 이 섹션은 Emacs 패키지 관리를 위해 `use-package`를 설정하여,
;; 필요한 패키지가 효율적으로 설치되고 구성되도록 합니다.

;; `use-package`가 설치되어 있는지 확인합니다. 설치되어 있지 않으면
;; 패키지 내용을 새로 고치고 설치합니다. `use-package`는 패키지 구성을 단순화합니다.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; `use-package` 라이브러리를 로드합니다.
(require 'use-package)

;; `ob-gptel` 패키지가 포함된 디렉토리를 Emacs의 로드 경로에 추가합니다.
;; 이는 Emacs가 패키지를 찾아 로드하는 데 필요합니다.
(add-to-list 'load-path "/Users/js/dotfiles/minimal/manual_packages/ob-gptel")

;; MELPA (Milkypostman's Emacs Lisp Package Archive) 저장소를
;; 패키지 아카이브 목록에 추가합니다. MELPA는 방대한 Emacs 커뮤니티
;; 기여 패키지 모음을 제공합니다. 끝에 있는 `t`는 목록의 끝에 추가함을 의미합니다.
(setq
 ;; 기본적으로 Emacs는 패키지를 설치할 때 관련 정보를 init.el에 기록합니다. 이를 방지하도록 설정합니다.
 package--init-file-ensured 't
 ;; org 관련 프로세스가 자동으로 종료될 수 있도록 합니다.
 kill-buffer-query-functions nil
 ;; 오래된 버전의 파일을 로드하지 않고 재컴파일하도록 합니다.
 load-prefer-newer t
 ;; 패키지 저장소 추가
 ;; 저장소: MELPA, MARMALADE, ORG-MODE, GNU, user42
 package-archives
 '(("melpa-stable" . "http://stable.melpa.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("gnu"         . "http://elpa.gnu.org/packages/")))



;; `use-package-always-ensure`를 `t`로 설정합니다. 이는 `use-package`로
;; 선언된 모든 패키지가 아직 존재하지 않으면 자동으로 설치되도록 보장하여
;; 패키지 관리를 단순화합니다.
(setq use-package-always-ensure t)

;;;; 3. 디스플레이 및 글꼴 설정

;; 이 설정들은 모드 라인 높이 및 글꼴 구성을 포함하여
;; Emacs의 시각적 모양을 제어합니다.

;; 활성 모드 라인의 높이를 설정합니다.
;; 모드 라인은 현재 버퍼에 대한 유용한 정보를 표시합니다.
(set-face-attribute 'mode-line nil :height 200)

;; 비활성 모드 라인의 높이를 설정합니다.
;; 비활성 모드 라인은 현재 선택되지 않은 다른 창에 속합니다.
(set-face-attribute 'mode-line-inactive nil :height 150)

;; --- D2Coding 글꼴 설정 ---
;; D2Coding을 Emacs의 기본 글꼴로 설정합니다.
;; 이 글꼴은 명확한 가독성으로 인해 프로그래밍에 인기가 있습니다.
(set-face-attribute 'default nil :font "D2Coding" :height 230)

;; 한글(한국어 문자)에 대해 D2Coding을 명시적으로 설정하여 적절한
;; 너비를 보장합니다. 이는 한글 문자가 영어 문자의 두 배 너비를 차지하는
;; 경우가 많으므로 일관된 정렬과 가독성에 중요합니다.
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))

;;;; 4. 핵심 필수 패키지

;; 이 섹션은 완성, 키 바인딩 도움말,
;; 구조적 편집과 같은 일반적인 작업에 대한 Emacs의 기능을 향상시키는
;; 필수 패키지를 구성합니다.

;; --- Vertico: 미니버퍼 완성 프레임워크 ---
;; `vertico`는 미니버퍼(M-x, C-x b 등 명령에 사용됨)를 향상시켜
;; 깔끔하고 효율적인 수직 완성 인터페이스를 제공합니다.
(use-package vertico
  :init
  ;; Vertico 모드를 전역적으로 활성화합니다.
  (vertico-mode)
  ;; 미니버퍼에서 위/아래로 이동할 때 후보를 순환하도록 설정합니다.
  ;; 이를 통해 완성 제안을 원활하게 탐색할 수 있습니다.
  (setq vertico-cycle t)
  :bind (:map vertico-map
        ;; Vertico에서 RET (Enter) 키의 동작을 사용자 정의합니다.
        ;; 선택된 후보가 디렉토리이면 해당 디렉토리로 이동하고,
        ;; 그렇지 않으면 선택을 완료합니다. 이는 파일 탐색을 간소화합니다.
        ("RET" . vertico-directory-enter)))

;; --- Orderless: 유연한 완성 스타일 ---
;; `orderless`는 부분 문자열의 순서에 관계없이 후보를 일치시킬 수 있는
;; 유연하고 강력한 완성 스타일을 제공합니다.
(use-package orderless
  :custom
  ;; 완성 스타일을 `orderless`를 먼저 사용하고, 그 다음 `basic`을 사용하도록 설정합니다.
  ;; `orderless`는 퍼지 매칭을 허용하고, `basic`은 정확한 접두사 매칭을 제공합니다.
  (completion-styles '(orderless basic))
  ;; 완성 스타일에 대한 카테고리별 기본값을 비활성화합니다.
  (completion-category-defaults nil)
  ;; 특정 카테고리에 대한 완성 스타일을 재정의합니다. 파일의 경우,
  ;; 파일 경로의 일부를 일치시킬 수 있도록 `partial-completion`을 사용합니다.
  (completion-category-overrides '((file (styles partial-completion)))))

;; --- Marginalia: 미니버퍼 주석 ---
;; `marginalia`는 미니버퍼의 완성 후보에 추가 정보(주석, 아이콘 등)를
;; 추가하여 선택을 더 유익하게 만듭니다.
(use-package marginalia
  :init
  ;; Marginalia 모드를 전역적으로 활성화합니다.
  (marginalia-mode))

;; --- Which-Key: 키 바인딩 도우미 ---
;; `which-key`는 접두사 키를 누른 후 사용 가능한 키 바인딩을 팝업 버퍼에
;; 표시하여 명령을 더 쉽게 찾을 수 있도록 합니다.
(use-package which-key
  :init
  ;; Which-Key 모드를 전역적으로 활성화합니다.
  (which-key-mode))

;; --- Expand-Region: 의미론적 선택 ---
;; `expand-region`은 Vim의 텍스트 객체와 유사한 "의미론적 선택" 기능을 제공하여
;; 논리적 단위(예: 단어, 문장, 단락, 함수)를 기반으로 선택을 확장할 수 있도록 합니다.
(use-package expand-region
  ;; `C-=`를 `er/expand-region`에 바인딩하여 현재 선택을 쉽게 확장할 수 있도록 합니다.
  :bind ("C-=" . er/expand-region))

;; --- Company: 자동 완성 프레임워크 ---
;; `company` (complete anything)는 모듈식 인-버퍼 완성 프레임워크로,
;; 입력하는 동안 실시간 완성 제안을 제공합니다.
(use-package company
  :init
  ;; 모든 버퍼에서 Company 모드를 전역적으로 활성화합니다.
  (global-company-mode)
  :config
  ;; 자동 완성 창이 나타나기 전 지연 시간을 0.2초로 설정합니다.
  (setq company-idle-delay 0.2)
  ;; 자동 완성 제안이 나타나기 전에 입력해야 하는 최소 문자 수를 설정합니다.
  (setq company-minimum-prefix-length 2)
  ;; Company 백엔드 목록에 `company-yasnippet`을 추가합니다. 이는
  ;; `yasnippet` (또 다른 스니펫)을 스니펫 완성에 통합합니다.
  (add-to-list 'company-backends 'company-yasnippet))


;; --- Flycheck: 실시간 구문 및 스타일 검사 ---
;; `flycheck`는 다양한 프로그래밍 언어에 대한 실시간 구문 검사 및 린팅을 제공하여
;; 버퍼에 직접 오류 및 경고를 강조 표시합니다.
(use-package flycheck
  :init
  ;; Flycheck 모드를 전역적으로 활성화합니다.
  (global-flycheck-mode))

;; --- Paredit: Lisp 코드의 구조적 편집 ---
;; `paredit`은 Lisp 코드의 구조적 편집을 위한 강력한 패키지로,
;; 균형 잡힌 괄호를 보장하고 일반적인 Lisp 편집 작업을 단순화합니다.
(use-package paredit
  ;; `emacs-lisp-mode`에서 Paredit 모드를 자동으로 활성화합니다.
  :hook (emacs-lisp-mode . paredit-mode)
  :bind (:map paredit-mode-map
              ;; `C-c >`를 `paredit-forward-slurp-sexp`에 바인딩하여
              ;; 다음 sexp를 슬러핑하여 현재 sexp를 확장합니다.
              ("C-c >" . paredit-forward-slurp-sexp)
              ;; `C-c <`를 `paredit-forward-barf-sexp`에 바인딩하여
              ;; 마지막 요소를 바핑하여 현재 sexp를 축소합니다.
              ("C-c <" . paredit-forward-barf-sexp)))

;; Emacs Lisp 모드에서 `eldoc-mode`를 활성화합니다. `eldoc-mode`는
;; 입력하는 동안 미니버퍼에 함수 시그니처와 문서를 표시하여
;; Lisp 개발에 매우 유용합니다.
(use-package eldoc
  :after emacs-lisp-mode)
;;(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; --- Projectile: 프로젝트 관리 ---
;; `projectile`은 Emacs용 프로젝트 상호 작용 라이브러리로,
;; 프로젝트 탐색, 검색 및 관리를 위한 편리한 명령을 제공합니다.
(use-package projectile
  :init
  ;; Projectile 모드를 전역적으로 활성화합니다.
  (projectile-mode +1)
  ;; 모드 라인을 깔끔하게 유지하기 위해 `projectile-mode` 표시기를 축소합니다.
  :diminish projectile-mode
  ;; 모든 Projectile 명령을 "C-c p" 접두사 키 아래로 그룹화합니다.
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; --- Vterm: 터미널 에뮬레이터 ---
;; `vterm`은 Emacs 내에서 완전한 기능을 갖춘 터미널 에뮬레이터로,
;; Emacs를 떠나지 않고 셸 명령을 실행하고 시스템과 상호 작용할 수 있도록 합니다.
(use-package vterm)

;; --- Yasnippet: 스니펫 확장 ---
;; `yasnippet`은 Emacs용 템플릿 시스템으로,
;; 자리 표시자가 있는 코드 스니펫을 정의하고 확장할 수 있도록 합니다.
(use-package yasnippet
  :init
  ;; `yasnippet`이 스니펫을 찾아야 하는 디렉토리를 구성합니다.
  ;; `user-emacs-directory`는 다른 시스템 간에 경로를 이식 가능하게 만듭니다.
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  ;; `yasnippet`을 전역적으로 활성화합니다.
  (yas-global-mode 1))

;; --- Yasnippet-Snippets: 스니펫 모음 ---
;; `yasnippet-snippets`는 `yasnippet`을 보완하는
;; 다양한 프로그래밍 언어에 대한 일반적인 코드 스니펫 모음을 제공합니다.
(use-package yasnippet-snippets
  ;; 이 패키지가 `yasnippet` 다음에 로드되도록 합니다.
  :after yasnippet)

;; --- Seoul256-Theme: 색상 테마 ---
;; `seoul256-theme`는 Emacs용 어두운 색상 테마로,
;; 시각적으로 매력적이고 편안한 코딩 환경을 제공합니다.
(use-package seoul256-theme
  :config
  ;; `seoul256` 테마의 배경색을 설정합니다.
  (setq seoul256-background 225)
  ;; `seoul256` 테마를 로드합니다. `t` 인수는 메시지를 억제함을 의미합니다.
  (load-theme 'seoul256 t)
  '(org-block ((t (:background "#545454" :extend t))))
  '(org-block-begin-line ((t (:background "##545454" :extend t))))
  '(org-block-end-line ((t (:background "##545454" :extend t))))
  '(org-code ((t (:background "#545454" :extend t)))))

;; --- Exec-Path-From-Shell: 셸 환경 통합 ---
;; `exec-path-from-shell`은 Emacs의 `exec-path` (Emacs가 실행 파일을 찾는 곳)를
;; 셸의 환경 변수와 동기화합니다. 이는 Emacs가 터미널과 동일한 프로그램을
;; 찾을 수 있도록 보장합니다.
(use-package exec-path-from-shell
  :config
  ;; Emacs를 GUI 환경(macOS, Windows, X11)에서 실행할 때만
  ;; `exec-path-from-shell`을 초기화합니다. 이는 터미널 Emacs에서 문제를 방지합니다.
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; --- Ripgrep: 빠른 Grep 도구 통합 ---
;; `ripgrep`은 현재 디렉토리를 재귀적으로 검색하여 정규식 패턴을 찾는
;; 라인 지향 검색 도구입니다. 이 패키지는 `ripgrep`을 Emacs에 통합하여
;; 빠르고 효율적인 검색을 가능하게 합니다.
(use-package ripgrep)

;;;; 5. Org Mode 설정

;; 이 섹션에는 Emacs의 강력한 일반 텍스트 개요, 메모 작성 및 게시 시스템인
;; Org Mode에 특정한 구성이 포함되어 있습니다.

;; --- Org-Bullets: 시각적 Org Mode 헤딩 ---
;; `org-bullets`는 기본 Org Mode 개요 별표를 더 시각적으로 매력적인
;; 유니코드 문자로 대체합니다.
(use-package org-bullets
  ;; `org-mode`에서 `org-bullets-mode`를 자동으로 활성화합니다.
  :hook (org-mode . org-bullets-mode)
  :config
  ;; 다른 수준의 Org Mode 헤딩에 사용할 유니코드 문자 목록을 정의합니다.
  (setq org-bullets-bullet-list '("●" "◉" "○" "◆" "▶" "▷" "▸")))

;; --- Helpful: 향상된 도움말 시스템 ---
;; `helpful`은 Emacs에 대한 더 사용자 친화적이고 유익한 도움말 시스템을 제공하여
;; 함수, 변수 및 키 바인딩을 더 쉽게 탐색할 수 있도록 합니다.
(use-package helpful
  :config
  ;; 표준 도움말 명령을 `helpful` 동등 명령으로 다시 매핑합니다.
  ;; `C-h f` (describe-function)는 이제 `helpful-callable`을 사용합니다.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  ;; `C-h v` (describe-variable)는 이제 `helpful-variable`을 사용합니다.
  (global-set-key (kbd "C-h v") #'helpful-variable)
  ;; `C-h k` (describe-key)는 이제 `helpful-key`를 사용합니다.
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; `C-h x` (describe-command)는 이제 `helpful-command`를 사용합니다.
  (global-set-key (kbd "C-h x") #'helpful-command))

;; Org Mode에서 `visual-line-mode`를 활성화합니다. 이는 긴 줄을
;; 실제 줄 바꿈을 삽입하지 않고 시각적으로 줄 바꿈하여 긴 단락의 가독성을 향상시킵니다.
(add-hook 'org-mode-hook 'visual-line-mode)

;; --- Org-Download: Org Mode용 이미지 다운로드 ---
;; `org-download`는 URL에서 이미지를 다운로드하거나 클립보드에서 캡처하여
;; Org Mode 파일에 이미지를 삽입하는 과정을 단순화합니다.
(use-package org-download
  :config
  ;; `dired-mode` (디렉토리 편집기)에서 `org-download` 기능을 활성화합니다.
  ;; 이를 통해 파일 브라우저에서 Org 파일로 이미지를 드래그 앤 드롭할 수 있습니다.
  (add-hook 'dired-mode-hook 'org-download-enable))


;; org mode에서 이미지의 크기를 원본으로 설정하지 않기 위함.
;; t로 설정하면 org mode에서 보여지는 이미지의 크기를 조정할 수 없음.
(setq  org-image-actual-width nil)




;; --- Org Mode Babel 설정 ---
;; Org Babel은 Org Mode 파일 내에서 코드 블록을 실행할 수 있도록 합니다.
;; 임의의 코드를 실행하는 것은 위험할 수 있으므로 보안상의 이유로
;; `org-confirm-babel-evaluate`를 `t`로 유지하는 것이 좋습니다.
;; (setq org-confirm-babel-evaluate nil) ; 이는 보안 위험입니다.

;; Org Mode의 소스 코드 블록에 대해 네이티브 폰트화를 활성화합니다.
;; 이는 코드 블록이 해당 언어의 주요 모드를 사용하여 강조 표시됨을 의미합니다.
(setq org-src-fontify-natively t)
;; 소스 코드 블록 내에서 Tab 키가 네이티브처럼 작동하도록 합니다.
;; 이는 Tab이 해당 언어 모드에 따라 들여쓰기를 삽입하거나 완성을 수행함을 의미합니다.
(setq org-src-tab-acts-natively t)
;; Org Babel에서 Python 코드 블록을 실행하는 데 사용할 명령을 지정합니다.
(setq org-babel-python-command "python3")


;;;; 6. AI 통합 (GPTel)

;; 이 섹션은 Gemini와 같은 대규모 언어 모델과 상호 작용하기 위한
;; Emacs 클라이언트인 `gptel`을 구성합니다.

;; --- GPTel: Gemini/LLM 클라이언트 ---
;; `gptel`을 사용하면 Emacs 내에서 직접 AI 모델과 상호 작용할 수 있습니다.
(use-package gptel
  :config
  ;; `gptel`과 함께 사용할 기본 모델을 설정합니다.
  (setq gptel-model 'gemini-2.5-flash)
  ;; API 키 및 스트리밍 응답 사용 여부를 포함하여 Gemini용 `gptel` 백엔드를 구성합니다.
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (car  (cdr  (auth-source-user-and-password "gemini")))
                        :stream nil))
  ;; AI 모델의 추론 과정을 표시하지 않도록 설정합니다.
  (setq gptel-show-reasoning nil)
  (require 'ob-gptel)

  ;; `ob-gptel` 패키지가 포함된 디렉토리를 Emacs의 로드 경로에 추가합니다.
  ;; 이는 Emacs가 패키지를 찾아 로드하는 데 필요합니다.
  ;;  (add-to-list 'load-path "/Users/js/dotfiles/minimal/manual_packages/ob-gptel")
  )

;; Org Babel이 다양한 언어에 대한 지원을 로드하도록 구성합니다.
;; 이는 해당 언어로 작성된 코드 블록의 실행을 가능하게 합니다.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)   ;; 셸 스크립트 실행 활성화
   (sql . t)     ;; SQL 실행 활성화
   (python . t)  ;; Python 실행 활성화
   (gptel . t))) ;; gptel (AI 상호 작용) 실행 활성화

;;;; 7. 기타 유틸리티

;; 이 섹션에는 다양한 기타 유용한 Emacs 유틸리티가 포함되어 있습니다.

;; `desktop-save-mode`를 활성화합니다. 이 모드는 Emacs 데스크톱의 상태(열린 파일,
;; 창 레이아웃 등)를 종료 시 저장하고 Emacs를 다시 시작할 때 복원합니다.
(desktop-save-mode 1)

;; --- Winner: 창 레이아웃 관리 ---
;; `winner`를 사용하면 창 레이아웃 변경을 실행 취소하고 다시 실행할 수 있습니다.
;; 이는 복잡한 창 구성을 탐색하는 데 매우 유용합니다.
(use-package winner
  ;; `winner` 패키지가 설치되어 있는지 확인합니다.
  :ensure t
  :config
  ;; `winner-mode`를 활성화합니다.
  (winner-mode))

;; --- Writeroom-Mode: 집중적인 글쓰기 ---
;; `writeroom-mode`는 현재 버퍼를 최대화하고 다른 UI 요소를 숨겨
;; 방해 없는 글쓰기 환경을 제공합니다.
(use-package writeroom-mode)


(use-package org-roam                                                   
  :ensure t                                                             
  :init                                                                 
  (setq org-roam-v2-ack t) ; v1에서 마이그레이션 경고를 비활성화합니다. 
  :custom                                                               
  (org-roam-directory "~/org")                                          
  (org-roam-completion-everywhere t)                                    
  :config                                                               
  (org-roam-db-autosync-mode)                                           
  :bind (("C-c n l" . org-roam-buffer-toggle)                           
         ("C-c n f" . org-roam-node-find)                               
         ("C-c n i" . org-roam-node-insert)))



;; (선택 사항) 이 새 함수를 원하는 키에 바인딩할 수 있습니다.
;; 예: C-c C-q 키 바인딩을 오버라이드하거나 다른 키를 사용합니다.
;; (define-key org-mode-map (kbd "C-c C-q") #'my-set-tags-and-create-id)
;; 또는 (define-key org-mode-map (kbd "C-c T") #'my-set-tags-and-create-id)

(use-package org-roam-ui
  ;; :straight
  ;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq completion-ignore-case t)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;;; minimal-init.el 끝

;;;; 8. 사용자 정의 변수 및 페이스

;; 이 섹션은 일반적으로 Emacs의 `customize` 인터페이스에서 관리됩니다.
;; 충돌을 피하기 위해 Emacs가 이 섹션을 처리하도록 하는 것이 좋습니다.
;; 수동으로 편집하면 문제가 발생할 수 있으므로 주의하십시오.
;; 이니셜 파일에는 이러한 인스턴스가 하나만 있어야 합니다.
;; 두 개 이상 있으면 제대로 작동하지 않습니다.


;; org mode에서 src code block의 색을 설정하는 부분
;; (use-package org
;;   :config
;;   (custom-set-faces
;;    ;; custom-set-faces was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;    '(org-block ((t (:background "#545454" :extend t))))
;;    '(org-block-begin-line ((t (:background "##545454" :extend t))))
;;    '(org-block-end-line ((t (:background "##545454" :extend t))))
;;    '(org-code ((t (:background "#545454" :extend t)))))

;;   (custom-set-variables
;;    )
;;   '(org-agenda-files
;;     '("~/org/software_developing_with_ai.org" "/Users/js/org/first.org"))
;;   '(org-image-actual-width '(300))) 
(setq user-full-name "Vimacs")
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(package-selected-packages
;;   '(yasnippet-snippets writeroom-mode which-key vterm vertico seoul256-theme ripgrep projectile paredit org-static-blog org-roam-ui org-present org-download org-bullets orderless marginalia magit-section htmlize helpful gptel flycheck expand-region exec-path-from-shell emacsql consult-org-roam company))
;; 보안상 필요한 코드
(setq safe-local-variable-values
      '((org-download-link-format . "[[%s]]")
        (org-download-image-dir . "~/blog/org/static")))

(defun vimacs/init/open-init-file ()
  "open currnetly being used init file"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'vimacs/init/open-init-file)


;;; 블로그 설정 로드
;;(load-file (expand-file-name "blog-config.el" user-emacs-directory))
;;(add-to-list 'load-path (expand-file-name "." user-emacs-directory))
(use-package blog-config
  :ensure nil
  :load-path ((lambda () 
                (list  (expand-file-name "modules" user-emacs-directory))))

  ;;:load-path  "{home}/dotfiles/minimal/modules"
  :commands (vimacs/update-index-with-posts vimacs/new-blog-post) ;; lazyloading - 이 함수들을 불러야 실제로 로딩이 된다.
  :config
  (message "blog 모듈이 로드되었습니다."))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-gutter-fringe git-gutter yasnippet-snippets writeroom-mode which-key vterm vertico seoul256-theme ripgrep projectile paredit org-roam-ui org-download org-bullets orderless marginalia htmlize helpful gptel flycheck expand-region exec-path-from-shell consult company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
