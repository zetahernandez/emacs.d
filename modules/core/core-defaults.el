;; -*- lexical-binding: t; -*-
;;; core-defaults.el --- Sensible defaults -*- lexical-binding: t -*-

;;; Commentary:
;; Encoding, backups, scrolling, and other defaults

;;; Code:

;; ============================================================
;; Encoding
;; ============================================================
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ============================================================
;; Backups & Auto-save
;; ============================================================
(defvar zeta/backup-dir (expand-file-name "backups" user-emacs-directory))
(defvar zeta/auto-save-dir (expand-file-name "auto-save" user-emacs-directory))

;; Crear directorios si no existen
(dolist (dir (list zeta/backup-dir zeta/auto-save-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(setq backup-directory-alist `(("." . ,zeta/backup-dir))
      auto-save-file-name-transforms `((".*" ,zeta/auto-save-dir t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ============================================================
;; UX Defaults
;; ============================================================
(setq ring-bell-function 'ignore           ; No bell
      use-short-answers t                   ; y/n en lugar de yes/no
      confirm-kill-emacs 'yes-or-no-p       ; Confirmar antes de salir
      require-final-newline t
      sentence-end-double-space nil
      tab-width 4
      indent-tabs-mode nil)                 ; Espacios, no tabs

;; ============================================================
;; Scrolling
;; ============================================================
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; ============================================================
;; Recentf (archivos recientes)
;; ============================================================
(use-package recentf
  :ensure nil  ; built-in
  :config
  (setq recentf-max-saved-items 100
        recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-exclude '("/tmp/" "/ssh:" "COMMIT_EDITMSG"))
  (recentf-mode 1))

;; ============================================================
;; Savehist (historial de minibuffer)
;; ============================================================
(use-package savehist
  :ensure nil  ; built-in
  :init
  (savehist-mode 1)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)
        savehist-additional-variables '(search-ring regexp-search-ring)))

;; ============================================================
;; Save place (recordar posición en archivos)
;; ============================================================
(use-package saveplace
  :ensure nil  ; built-in
  :init
  (save-place-mode 1))

;; ============================================================
;; Exec path from shell (para emacs --daemon)
;; ============================================================
(use-package exec-path-from-shell
  :if (daemonp)
  :config
  (exec-path-from-shell-initialize))

;; ============================================================
;; Tree-sitter grammars path
;; ============================================================
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))

(provide 'core-defaults)
;;; core-defaults.el ends here
