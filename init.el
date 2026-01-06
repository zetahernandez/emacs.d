;; -*- lexical-binding: t; -*-
;;; init.el --- Zeta's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modular Emacs configuration for Emacs 30.2+

;;; Code:

;; Directorio de módulos
(defvar zeta/modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory containing configuration modules.")

;; Función para cargar módulos
(defun zeta/load-module (category module)
  "Load MODULE from CATEGORY directory."
  (let ((file (expand-file-name (format "%s/%s" category module) zeta/modules-dir)))
    (load file nil 'nomessage)))

;; ============================================================
;; Core modules
;; ============================================================
(zeta/load-module "core" "core-packages")
(zeta/load-module "core" "core-defaults")
(zeta/load-module "core" "core-ui")
(zeta/load-module "core" "core-keybindings")

;; ============================================================
;; Completion modules
;; ============================================================
(zeta/load-module "completion" "completion-vertico")
(zeta/load-module "completion" "completion-consult")
(zeta/load-module "completion" "completion-corfu")

;; ============================================================
;; Editor modules
;; ============================================================
(zeta/load-module "editor" "editor-defaults")
(zeta/load-module "editor" "editor-snippets")

;; ============================================================
;; Tools modules
;; ============================================================
(zeta/load-module "tools" "tools-magit")
(zeta/load-module "tools" "tools-project")
(zeta/load-module "tools" "tools-treemacs")

;; ============================================================
;; Org modules (Fase 5 - descomentar después)
;; ============================================================
;; (zeta/load-module "org" "org-config")

;; ============================================================
;; Language modules
;; ============================================================
(zeta/load-module "lang" "lang-lsp")
(zeta/load-module "lang" "lang-python")
(zeta/load-module "lang" "lang-typescript")
(zeta/load-module "lang" "lang-markdown")

;; ============================================================
;; Post-init
;; ============================================================

;; Restaurar GC después de startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
