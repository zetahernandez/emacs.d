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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-safe-themes
   '("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(org-agenda-files
   '("~/Dropbox/orgfiles/i.org" "~/Dropbox/orgfiles/schedule.org"))
 '(org-support-shift-select t)
 '(package-selected-packages
   '(docker-compose-mode flycheck-yamllint apparmor-mode prettier-js pyimport git-link terraform-mode go-mode company-web ac-html-bootstrap ac-html-angular emmet-mode web-mode-edit-element htmlize helm-company elpy ox-rst ob-restclient diff-hl helm-gitignore gitignore-mode copy-as-format powerline dashboard all-the-icons engine-mode helm-ls-git helm-ag treemacs-icons-dired treemacs-magit treemacs-projectile treemacs helm-projectile projectile helm js-comint company-tern tern exec-path-from-shell yasnippet-snippets web-mode use-package smex smartparens rjsx-mode restclient py-isort move-text material-theme markdown-mode kubernetes k8s-mode indium hlinum flycheck dockerfile-mode docker))
 '(prettier-js-command
   "~/flieber/flieber-be/frontend-app/node_modules/prettier/bin-prettier.js")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "light salmon")))))
(put 'scroll-left 'disabled nil)
