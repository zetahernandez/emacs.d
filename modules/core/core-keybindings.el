;; -*- lexical-binding: t; -*-
;;; core-keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Core keybindings that don't depend on specific packages

;;; Code:

;; ============================================================
;; Window navigation
;; ============================================================
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; ============================================================
;; Undo/Redo (built-in en Emacs 28+)
;; ============================================================
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; ============================================================
;; Buffer management
;; ============================================================
(global-set-key (kbd "C-x k") 'kill-current-buffer)  ; Más rápido que kill-buffer

;; ============================================================
;; Text scaling
;; ============================================================
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; ============================================================
;; Better defaults
;; ============================================================
;; Revertir buffer automáticamente si cambia en disco
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Delete selection mode (reemplazar texto seleccionado al escribir)
(delete-selection-mode 1)

;; Electric pair mode (cerrar paréntesis automáticamente)
(electric-pair-mode 1)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

(provide 'core-keybindings)
;;; core-keybindings.el ends here
