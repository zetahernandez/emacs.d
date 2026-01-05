;; -*- lexical-binding: t; -*-
;;; completion-vertico.el --- Vertico completion UI -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico + Orderless + Marginalia for minibuffer completion
;; Replaces Helm's minibuffer functionality

;;; Code:

;; ============================================================
;; Vertico - Vertical completion UI
;; ============================================================
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)                 ; Cycle through candidates
  (vertico-count 15)                ; Number of candidates to show
  (vertico-resize nil)              ; Don't resize minibuffer
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)))

;; Vertico directory extension (better directory navigation)
(use-package vertico-directory
  :after vertico
  :ensure nil  ; Part of vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================
;; Orderless - Flexible matching
;; ============================================================
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; ============================================================
;; Marginalia - Rich annotations
;; ============================================================
(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; ============================================================
;; Savehist - Persist minibuffer history
;; ============================================================
;; Already configured in core-defaults.el, but ensure it's on
(savehist-mode 1)

(provide 'completion-vertico)
;;; completion-vertico.el ends here
