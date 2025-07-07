;;; init.el --- Emacs initialization file

;; This file is the main init file.
;; It loads config.org if it exists in the same directory.

(let ((config-org-path (expand-file-name "config.org" (file-name-directory (file-truename load-file-name)))))
  (when (file-exists-p config-org-path)
    (org-babel-load-file config-org-path)))

