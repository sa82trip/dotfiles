;;; blog-config.el --- Org Mode 블로그 운영 설정 ---

;; 이 파일은 Emacs Org Mode를 사용하여 블로그를 운영하기 위한
;; 모든 관련 설정을 포함합니다.

(defvar my/blog-org-directory (expand-file-name "~/blog/org/")
  "블로그 org 파일들이 저장된 기본 디렉토리입니다.")

(defvar my/blog-posts-directory (concat my/blog-org-directory "posts/")
  "블로그 포스트 파일들이 저장된 디렉토리입니다.")

(defvar my/blog-template-directory (concat my/blog-org-directory "templates/")
  "블로그 템플릿 파일들이 저장된 디렉토리입니다.")

;; 디렉토리 생성 확인
(unless (file-exists-p my/blog-posts-directory)
  (make-directory my/blog-posts-directory t))

(unless (file-exists-p my/blog-template-directory)
  (make-directory my/blog-template-directory t))

(setq org-publish-project-alist
      `(("blog-org"
         :base-directory ,my/blog-org-directory
         :base-extension "org"
         :publishing-directory "~/blog/public/"
         :recursive t
         :exclude "\\.Trash\\|\\.git\\|\\.DS_Store"
         :publishing-function org-html-publish-to-html
         :with-toc t            ; 목차 출력
         :section-numbers nil   ; 섹션 넘버 감춤
         :htmlized-source t     ; 코드 하이라이트
         :auto-preamble t
         :with-author nil       ; Don't include author name
         :with-creator t)       ; Include Emacs and Org versions in footer
        ("blog-static"
         :base-directory ,(concat my/blog-org-directory "static/")
         :base-extension "jpg\\|jpeg\\|gif\\|png\\|svg"
         :publishing-directory "~/blog/public/static/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-assets"
         :base-directory "~/blog/assets/"
         :base-extension "css\\|js"
         :publishing-directory "~/blog/public/assets/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog"
         :components ("blog-org" "blog-static" "blog-assets"))))

;; Customize the HTML output
(setq org-html-validation-link nil      ; Don't show validation link
      org-html-head-include-scripts nil ; Use our own scripts
      org-html-head-include-default-style nil ; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\" />
                     <script defer src=\"/assets/search.js\"></script>")

;; simple한 서버를 띄워서 블로그 확인
;; (use-package simple-httpd :ensure t)
(setq httpd-root "~/blog/public")
;; (httpd-start)

;; HTML 특수문자 이스케이프 함수
(defun my/escape-html (string)
  "HTML 특수문자를 이스케이프합니다."
  (let ((replacements '(("&" . "&amp;")
                       ("<" . "&lt;")
                       (">" . "&gt;")
                       ("\"" . "&quot;")
                       ("'" . "&#39;"))))
    (dolist (pair replacements string)
      (setq string (replace-regexp-in-string (car pair) (cdr pair) string)))))

;; 키워드 값을 가져오는 도우미 함수
(defun my/org-get-keyword (keyword)
  "현재 버퍼에서 Org 키워드(#+KEYWORD: value)의 값을 반환합니다."
  (let ((case-fold-search t)) ; 대소문자 구분 없이 검색
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s: *\\(.*\\)" keyword) nil t)
        (match-string-no-properties 1)))))

;; 파일 수정 시간으로 정렬하는 함수
(defun my/sort-files-by-mtime (files)
  "파일 목록을 수정 시간 기준으로 정렬합니다 (최신 파일이 먼저)."
  (sort files
        (lambda (a b)
          (time-less-p (file-attribute-modification-time (file-attributes b))
                       (file-attribute-modification-time (file-attributes a))))))

;; 메인 함수 (수정된 버전)
(defun my/update-index-with-posts ()
  "posts/ 디렉토리의 org 파일들을 읽어 index.org에 최신 글 목록을 업데이트합니다."
  (interactive)
  (let* ((index-file (concat my/blog-org-directory "index.org"))
         (post-files (my/sort-files-by-mtime 
                      (directory-files my/blog-posts-directory t "\\.org$")))
         (post-list-html ""))
    (dolist (file post-files)
      (with-temp-buffer
        (insert-file-contents file)
        (unless (equal (my/org-get-keyword "DRAFT") "TRUE")
          (let* ((title (or (my/org-get-keyword "TITLE") "제목 없음"))
                 (title-escaped (my/escape-html title))
                 (date-raw (or (my/org-get-keyword "DATE") ""))
                 (date (replace-regexp-in-string "[<>]" "" date-raw))
                 (filename (file-name-nondirectory (file-name-sans-extension file)))
                 (link (format "./posts/%s.html" filename)))
            (setq post-list-html
                  (concat post-list-html
                          (format "<li data-title=\"%s\"><a href=\"%s\">%s</a> (%s)</li>\n"
                                  title-escaped link title-escaped date)))))))
    (with-temp-buffer
      (insert
       "#+TITLE: Vimacs's Story\n"
       "#+AUTHOR: Vimacs\n\n"
       "#+BEGIN_EXPORT html\n"
       "<input type=\"text\" id=\"search-input\" placeholder=\"Search\">\n"
       "#+END_EXPORT\n\n"
       "최신 글 목록:\n\n"
       "#+BEGIN_EXPORT html\n"
       "<ul id=\"post-list\">\n"
       post-list-html
       "</ul>\n"
       "<p id=\"no-results\" style=\"display: none;\">검색 결과가 없습니다.</p>\n"
       "#+END_EXPORT\n")
      (write-file index-file))
    (message "Index 페이지가 업데이트되었습니다.")))

(defun my/new-blog-post (title)
  "posts 폴더에 새로운 블로그 포스트를 템플릿 파일을 기반으로 만들어 엽니다."
  (interactive "sPost title: ")
  (let* ((slug (downcase title))
         ;; 1. 한글 및 허용된 문자 외의 것들을 하이픈으로 변경
         (slug (replace-regexp-in-string "[^a-z0-9_가-힣-]" "-" slug))
         ;; 2. 여러 개의 하이픈을 하나로 축약
         (slug (replace-regexp-in-string "--+" "-" slug))
         ;; 3. 맨 앞/뒤 하이픈 제거
         (slug (replace-regexp-in-string "^-\\|-$" "" slug))
         (date (format-time-string "%Y-%m-%d"))
         (filename (format "%s-%s.org" date slug))
         (full-path (expand-file-name filename my/blog-posts-directory))
         (template-path (expand-file-name "blog-post-template.org" my/blog-template-directory)))
    (if (file-exists-p full-path)
        (message "이미 같은 이름의 포스트가 존재합니다: %s" filename)
      (progn
        ;; 템플릿 파일이 없으면 기본 템플릿 생성
        (unless (file-exists-p template-path)
          (with-temp-file template-path
            (insert "#+TITLE: {{TITLE}}\n"
                    "#+DATE: <{{DATE}}>\n"
                    "#+AUTHOR: Vimacs\n"
                    "#+DRAFT: FALSE\n\n"
                    "* 서론\n\n"
                    "* 본론\n\n"
                    "* 결론\n")))
        ;; 새 포스트 파일 생성
        (find-file full-path)
        ;; 템플릿 파일 내용을 버퍼에 삽입
        (insert-file-contents template-path)
        ;; 버퍼 전체를 대상으로 플레이스홀더를 실제 값으로 치환
        (goto-char (point-min))
        (while (search-forward "{{TITLE}}" nil t)
          (replace-match title nil t))
        (goto-char (point-min))
        (while (search-forward "{{DATE}}" nil t)
          (replace-match date nil t))
        (save-buffer)
        (message "Created new post: %s" filename)))))

;; 블로그 발행 함수
(defun my/publish-blog ()
  "블로그 포스트들을 HTML로 발행하고 인덱스를 업데이트합니다."
  (interactive)
  (my/update-index-with-posts)
  (org-publish "blog" t)
  (message "블로그가 발행되었습니다."))

;; 편의 함수들
(defun my/blog-open-index ()
  "블로그 인덱스 파일을 엽니다."
  (interactive)
  (find-file (concat my/blog-org-directory "index.org")))

(defun my/blog-dired-posts ()
  "posts 디렉토리를 dired로 엽니다."
  (interactive)
  (dired my/blog-posts-directory))

(provide 'blog-config)
;;; blog-config.el ends here



