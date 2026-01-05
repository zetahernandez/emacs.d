;; -*- lexical-binding: t; -*-
;;; editor-defaults.el --- Editor enhancements -*- lexical-binding: t -*-

;;; Commentary:
;; Multiple cursors, text manipulation, and editing utilities

;;; Code:

;; ============================================================
;; Multiple Cursors
;; ============================================================
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C->"     . mc/mark-all-like-this)
   ("C-c C-<"     . mc/mark-all-like-this-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/always-run-for-all t))

;; ============================================================
;; Move text up/down
;; ============================================================
(use-package move-text
  :bind
  (("M-S-<up>"   . move-text-up)
   ("M-S-<down>" . move-text-down)))

;; ============================================================
;; Rainbow delimiters (colorear paréntesis)
;; ============================================================
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;; Smartparens (manejo inteligente de paréntesis)
;; ============================================================
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (with-no-warnings
    (require 'smartparens-config)))

;; ============================================================
;; Whitespace cleanup
;; ============================================================
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ============================================================
;; Duplicate line/region
;; ============================================================
(defun zeta/duplicate-line-or-region (arg)
  "Duplicate current line or region ARG times."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (_ arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'zeta/duplicate-line-or-region)

;; ============================================================
;; Expand region (selección incremental)
;; ============================================================
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(provide 'editor-defaults)
;;; editor-defaults.el ends here
