;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration
;; file or tangling and loading a literate org configuration file.

;; Don't attempt to find/apply special file handlers to files loaded during
;; startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "main.elc" user-emacs-directory))
      (load-file (expand-file-name "main.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "main.org" user-emacs-directory))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (company-web ac-html-bootstrap ac-html-angular emmet-mode web-mode-edit-element htmlize helm-company elpy ox-rst ob-restclient diff-hl helm-gitignore gitignore-mode copy-as-format powerline dashboard all-the-icons engine-mode helm-ls-git helm-ag treemacs-icons-dired treemacs-magit treemacs-projectile treemacs helm-projectile projectile helm js-comint company-tern tern exec-path-from-shell yasnippet-snippets web-mode use-package smex smartparens rjsx-mode restclient py-isort move-text material-theme markdown-mode kubernetes k8s-mode indium hlinum flycheck dockerfile-mode docker))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
