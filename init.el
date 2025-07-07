;;; init.el --- Emacs initialization file

;; This file is the main init file.
;; It loads config.org if it exists in the same directory.

(let ((config-org-path (expand-file-name "config.org" (file-name-directory (file-truename load-file-name)))))
  (when (file-exists-p config-org-path)
    (org-babel-load-file config-org-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(non-existent-package-for-test vterm which-key projectile general evil-surround evil-collection doom-themes counsel company cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
