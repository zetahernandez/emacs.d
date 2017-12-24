;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-extension)
(require 'base-theme)
(require 'base-global-keys)
(require 'lang-python)
(require 'lang-javascript)
(require 'lang-web)
(require 'flycheck-js)
;; (require 'elpy-eventbrite)
(require 'python-functions)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode t)
 '(custom-safe-themes
   (quote
    ("0bfe81f0ddc788922f718b3320991320fa46a7ac7723e5f98967192e8d0393f8" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(helm-projectile-fuzzy-match nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (helm-spotify fiplr elpy protobuf-mode py-isort exec-path-from-shell github-browse-file company-web ac-html-bootstrap ac-html-angular emmet-mode web-mode-edit-element use-package)))
 '(projectile-enable-caching t)
 '(projectile-mode t nil (projectile))
 '(restclient-inhibit-cookies t)
 '(tls-program
   (quote
    ("gnutls-cli --insecure -p %p %h" "gnutls-cli --x509cafile %t -p %p %h" "gnutls-cli --x509cafile %t -p %p %h --protocols ssl3" "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
