;; -*- lexical-binding: t; -*-
;;; tools-magit.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and git-related tools

;;; Code:

;; ============================================================
;; Magit - Git interface
;; ============================================================
(use-package magit
  :bind
  (("C-x g s" . magit-status)
   ("C-x g c" . magit-commit)
   ("C-x g p" . magit-push)
   ("C-x g u" . magit-pull)
   ("C-x g l" . magit-log)
   ("C-x g b" . magit-blame)
   ("C-x g d" . magit-diff))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ============================================================
;; diff-hl - Highlight uncommitted changes
;; ============================================================
(use-package diff-hl
  :hook
  ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode))
  :config
  (global-diff-hl-mode))

;; ============================================================
;; Git modes (gitignore, gitconfig, etc.)
;; ============================================================
(use-package git-modes)

(provide 'tools-magit)
;;; tools-magit.el ends here
