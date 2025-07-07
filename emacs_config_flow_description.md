# Emacs 설정 로드 흐름 설명

이 문서는 Emacs가 시작될 때 설정 파일이 로드되고 적용되는 과정을 단계별로 설명합니다.

1.  **Emacs 시작**: Emacs 애플리케이션이 실행됩니다.
2.  **init.el 로드**: Emacs는 초기화 파일인 `init.el`을 로드합니다. 이 파일은 Emacs 설정의 진입점 역할을 합니다.
3.  **init.el 내용**: `init.el` 파일은 직접적인 설정 코드 대신, `config.org` 파일을 처리하도록 지시하는 코드를 포함합니다.
4.  **org-babel-load-file 호출**: `init.el` 내의 `(org-babel-load-file "config.org")` 함수가 호출됩니다. 이 함수는 `config.org` 파일을 읽고 Org-mode의 Babel 기능을 사용하여 해당 파일 내의 Emacs Lisp 소스 코드 블록을 "엮는(tangle)" 과정을 시작합니다.
5.  **config.org 엮기 (Tangle)**: `config.org` 파일 내에 정의된 모든 `emacs-lisp` 소스 코드 블록이 추출되어 하나의 Emacs Lisp 파일로 변환됩니다. 이 과정에서 Org-mode의 주석이나 문서화 내용은 무시되고 순수한 코드만 추출됩니다.
6.  **생성된 Emacs Lisp 코드**: 엮기 과정을 통해 생성된 Emacs Lisp 코드가 메모리에 로드되거나, 일반적으로 `init.el`과 동일한 디렉토리에 `config.el`과 같은 이름으로 저장됩니다. (현재 설정에서는 `init.el`이 `config.org`를 직접 로드하고 엮으므로 별도의 `config.el` 파일이 생성되지 않고 메모리에서 처리됩니다.)
7.  **설정 실행**: 추출된 Emacs Lisp 코드가 Emacs 환경 내에서 순차적으로 실행됩니다. 이 과정에서 정의된 모든 설정과 패키지가 로드되고 활성화됩니다.
8.  **패키지 관리 및 로드 (use-package)**:
    -   **패키지 다운로드**: Emacs는 `package-archives`에 설정된 저장소(예: MELPA, GNU ELPA)에서 패키지를 다운로드합니다. 다운로드된 패키지는 일반적으로 Emacs의 사용자 설정 디렉토리 내의 `elpa` 서브디렉토리(예: `~/.emacs.d/elpa/`)에 저장됩니다.
    -   **`package-initialize`**: Emacs 시작 시 `package-initialize` 함수가 호출되어 `elpa` 디렉토리에 있는 설치된 패키지들을 로드 경로에 추가하고 초기화합니다.
    -   **`use-package`의 역할**: `use-package` 매크로는 특정 패키지가 설치되어 있는지 확인하고, 설치되어 있지 않다면 자동으로 다운로드 및 설치를 지시합니다(`:ensure t` 옵션). 패키지가 로드된 후에는 `use-package` 블록 내의 `:init` (패키지 로드 전 실행) 및 `:config` (패키지 로드 후 실행) 섹션에 정의된 Emacs Lisp 코드를 실행하여 해당 패키지를 활성화하고 사용자 정의 설정을 적용합니다.
9.  **기본 설정**: `global-display-line-numbers-mode`와 같은 기본적인 Emacs 설정이 적용됩니다.
10. **테마 (doom-themes)**: `doom-themes` 패키지가 로드되고 `doom-one` 테마가 활성화됩니다.
11. **탐색 및 자동 완성 (ivy, counsel, projectile, company)**: `ivy`, `counsel`, `projectile`, `company`와 같은 탐색 및 자동 완성 관련 패키지들이 로드되고 설정됩니다.
12. **Evil 모드 (evil, evil-collection, evil-surround)**: Vim 키바인딩을 제공하는 `evil` 모드와 관련 확장 패키지(`evil-collection`, `evil-surround`)가 로드되고, `jk`를 이용한 노멀 모드 전환과 같은 Evil 관련 키바인딩이 설정됩니다.
13. **General 키바인딩**: `general` 패키지를 사용하여 정의된 전역 키바인딩, 특히 `SPC` (스페이스바)를 접두사로 사용하는 리더 키바인딩이 설정됩니다.
14. **사용자 정의 변수 및 얼굴**: `custom-set-variables` 및 `custom-set-faces`를 통해 사용자가 정의한 변수 및 폰트/색상 설정이 적용됩니다.
15. **Clojure 설정 (cider)**: Clojure 개발 환경을 위한 `cider` 패키지가 로드되고, REPL 자동 네임스페이스 전환 및 `lein` 실행 경로 설정과 같은 Clojure 관련 설정이 적용됩니다.

이러한 과정을 통해 Emacs는 사용자가 `config.org` 파일에 정의한 모든 설정을 로드하고 적용하여 개인화된 개발 환경을 구축합니다.
