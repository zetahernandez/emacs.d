;; -*- lexical-binding: t; -*-
;;; core-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, fonts, and visual elements

;;; Code:

;; ============================================================
;; Disable UI elements (respaldo si early-init no los deshabilitó)
;; ============================================================
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; ============================================================
;; Line numbers
;; ============================================================
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type t)  ; t = absolutos, 'relative = relativos

;; Deshabilitar line numbers en algunos modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ============================================================
;; Frame title
;; ============================================================
(setq frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

;; ============================================================
;; Highlight current line
;; ============================================================
(global-hl-line-mode 1)

;; ============================================================
;; Theme
;; ============================================================
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dark+ t)

  ;; Mejorar org-mode fontification
  (doom-themes-org-config))

;; ============================================================
;; Modeline
;; ============================================================
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-minor-modes t))

;; Minions para agrupar minor modes
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :config
  (setq minions-mode-line-lighter ""))

;; ============================================================
;; Icons
;; ============================================================
(use-package all-the-icons
  :if (display-graphic-p))

;; Nerd icons (alternativa más moderna)
(use-package nerd-icons
  :if (display-graphic-p))

;; ============================================================
;; Which-key (mostrar keybindings disponibles)
;; ============================================================
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05))

;; ============================================================
;; Font
;; ============================================================
(defvar zeta/default-font-size 140)  ; 14pt

(set-face-attribute 'default nil :height zeta/default-font-size)

;; Función para cambiar tamaño de fuente
(defun zeta/set-font-size (size)
  "Set font SIZE (in points * 10)."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height size))

(provide 'core-ui)
;;; core-ui.el ends here
