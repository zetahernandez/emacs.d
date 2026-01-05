;; -*- lexical-binding: t; -*-
;;; completion-corfu.el --- Corfu in-buffer completion -*- lexical-binding: t -*-

;;; Commentary:
;; Corfu + Cape for in-buffer completion
;; Replaces Company-mode

;;; Code:

;; ============================================================
;; Corfu - In-buffer completion UI
;; ============================================================
(use-package corfu
  :custom
  (corfu-cycle t)                  ; Cycle through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-delay 0.2)           ; Delay before showing (was company-idle-delay)
  (corfu-auto-prefix 2)            ; Min chars before showing (was company-minimum-prefix-length)
  (corfu-quit-no-match 'separator) ; Quit if no match
  (corfu-preview-current nil)      ; Don't preview current candidate
  (corfu-preselect 'prompt)        ; Don't preselect first candidate
  (corfu-on-exact-match nil)       ; Don't auto-insert on exact match
  (corfu-scroll-margin 5)

  :bind
  (:map corfu-map
        ("TAB"     . corfu-next)
        ([tab]     . corfu-next)
        ("S-TAB"   . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET"     . corfu-insert)
        ("C-g"     . corfu-quit))

  :init
  (global-corfu-mode))

;; ============================================================
;; Corfu extensions
;; ============================================================

;; Show documentation popup
(use-package corfu-popupinfo
  :ensure nil  ; Part of corfu
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.4 . 0.2)))

;; ============================================================
;; Cape - Completion At Point Extensions
;; ============================================================
(use-package cape
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)

  :bind
  (("C-c p p" . completion-at-point) ; capf
   ("C-c p d" . cape-dabbrev)        ; dabbrev
   ("C-c p f" . cape-file)           ; file
   ("C-c p k" . cape-keyword)        ; keyword
   ("C-c p s" . cape-elisp-symbol)   ; elisp symbol
   ("C-c p a" . cape-abbrev)         ; abbrev
   ("C-c p l" . cape-line)           ; line
   ("C-c p w" . cape-dict)))         ; dictionary

;; ============================================================
;; Kind-icon - Icons in completion (uses SVG)
;; ============================================================
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ============================================================
;; Emacs built-in completion settings
;; ============================================================
(setq tab-always-indent 'complete)

;; Completion-at-point settings
(setq completion-cycle-threshold 3)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(provide 'completion-corfu)
;;; completion-corfu.el ends here
