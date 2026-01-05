;; -*- lexical-binding: t; -*-
;;; lang-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Base LSP configuration using lsp-mode

;;; Code:

;; ============================================================
;; LSP Mode
;; ============================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; Performance tuning
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-completion-provider :none)  ; Use corfu instead

  ;; UI preferences
  (lsp-headerline-breadcrumb-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)

  ;; Disable features handled by other tools
  (lsp-enable-snippet nil)  ; We use yasnippet separately

  ;; File watchers (can be slow on large projects)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 2000)

  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l R" . lsp-find-references))

  :hook
  (lsp-mode . lsp-enable-which-key-integration)

  :config
  ;; Integrate with corfu for completions
  (setq lsp-completion-provider :none)

  (defun zeta/lsp-mode-setup-completion ()
    "Configure completion for lsp-mode with corfu."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'lsp-completion-mode-hook #'zeta/lsp-mode-setup-completion))

;; ============================================================
;; LSP UI - Enhanced UI for lsp-mode
;; ============================================================
(use-package lsp-ui
  :after lsp-mode
  :custom
  ;; Sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.5)

  ;; Peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)

  ;; Doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)

  :bind (:map lsp-mode-map
              ("C-c l p" . lsp-ui-peek-find-references)
              ("C-c l D" . lsp-ui-doc-glance)))

;; ============================================================
;; Consult integration with LSP
;; ============================================================
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l e" . consult-lsp-diagnostics)))

(provide 'lang-lsp)
;;; lang-lsp.el ends here
