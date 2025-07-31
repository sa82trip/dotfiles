
;;; blog-config-test.el --- Unit tests for blog-config.el

(require 'ert)
(require 'cl-lib)

;; 테스트 환경 설정
(defvar my/blog-test-temp-dir (expand-file-name "test-blog-temp/" temporary-file-directory)
  "테스트를 위한 임시 블로그 디렉토리입니다.")

;; 테스트용 경로 변수 설정 (let 바인딩으로 각 테스트 내에서 격리)
(defvar my/blog-org-directory)
(defvar my/blog-posts-directory)

;; `blog-config.el` 로드
(load-file (expand-file-name "../blog-config.el" (file-name-directory load-file-name)))

(ert-deftest test-my-new-blog-post ()
  "my/new-blog-post 함수가 올바른 파일명과 내용으로 새 포스트를 생성하는지 테스트합니다."
  (let ((my/blog-org-directory (concat my/blog-test-temp-dir "org/"))
        (my/blog-posts-directory (concat my/blog-test-temp-dir "org/posts/"))
        (user-emacs-directory (concat my/blog-test-temp-dir "emacs.d/")))
    ;; :SETUP
    (make-directory my/blog-posts-directory t)
    (make-directory (concat user-emacs-directory "minimal/templates/") t)
    (with-temp-buffer
      (write-file (concat user-emacs-directory "minimal/templates/blog-post-template.org"))
      (insert "#+TITLE: {{TITLE}}\n#+DATE: {{DATE}}\n#+DRAFT: TRUE\n#+OPTIONS: toc:nil num:nil\n\n* 서론\n\n* 본문\n\n* 마무리\n"))
    (let* ((title "테스트 포스트! (Test Post)")
           (date (format-time-string "%Y-%m-%d"))
           (expected-slug "테스트-포스트-test-post")
           (expected-filename (format "%s-%s.org" date expected-slug))
           (expected-filepath (expand-file-name expected-filename my/blog-posts-directory)))
      
      ;; :BODY
      (my/new-blog-post title)

      ;; :ASSERT
      (should (file-exists-p expected-filepath))
      (let ((file-content (with-temp-buffer (insert-file-contents expected-filepath) (buffer-string))))
        (should (string-match (format "#+TITLE: %s" (regexp-quote title)) file-content))
        (should (string-match (format "#+DATE: %s" (regexp-quote date)) file-content))
        (should (string-match "#+DRAFT: TRUE" file-content))))

    ;; :TEARDOWN
    (delete-directory my/blog-test-temp-dir t)))

(ert-deftest test-my-update-index-with-posts ()
  "my/update-index-with-posts가 공개된 포스트만 인덱스에 추가하는지 테스트합니다."
  (let ((my/blog-org-directory (concat my/blog-test-temp-dir "org/"))
        (my/blog-posts-directory (concat my/blog-test-temp-dir "org/posts/"))
        (user-emacs-directory (concat my/blog-test-temp-dir "emacs.d/")))
    ;; :SETUP
    (make-directory my/blog-posts-directory t)
    (make-directory (concat user-emacs-directory "minimal/templates/") t)
    (with-temp-buffer
      (write-file (concat user-emacs-directory "minimal/templates/blog-post-template.org"))
      (insert "#+TITLE: {{TITLE}}\n#+DATE: {{DATE}}\n#+DRAFT: TRUE\n#+OPTIONS: toc:nil num:nil\n\n* 서론\n\n* 본문\n\n* 마무리\n"))
    ;; 1. 공개 포스트 (DRAFT 없음)
    (with-temp-buffer
      (write-file (expand-file-name "public-post.org" my/blog-posts-directory))
      (insert "#+TITLE: Public Post\n#+DATE: 2025-01-01"))
    ;; 2. 비공개 포스트 (DRAFT: TRUE)
    (with-temp-buffer
      (write-file (expand-file-name "draft-post.org" my/blog-posts-directory))
      (insert "#+TITLE: Draft Post\n#+DATE: 2025-01-02\n#+DRAFT: TRUE"))
    ;; 3. 공개 포스트 (DRAFT: FALSE)
    (with-temp-buffer
      (write-file (expand-file-name "public-post-2.org" my/blog-posts-directory))
      (insert "#+TITLE: Another Public Post\n#+DATE: 2025-01-03\n#+DRAFT: FALSE"))

    ;; :BODY
    (my/update-index-with-posts)

    ;; :ASSERT
    (let ((index-file (expand-file-name "index.org" my/blog-org-directory)))
      (should (file-exists-p index-file))
      (let ((index-content (with-temp-buffer (insert-file-contents index-file) (buffer-string))))
        (should (string-match "Public Post" index-content))
        (should (string-match "Another Public Post" index-content))
        (should-not (string-match "Draft Post" index-content))))

    ;; :TEARDOWN
    (delete-directory my/blog-test-temp-dir t))))
