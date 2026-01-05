;; -*- lexical-binding: t; -*-
;;; lang-markdown.el --- Markdown and Mermaid configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing with preview and Mermaid diagram support

;;; Code:

;; ============================================================
;; Markdown Mode
;; ============================================================
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))  ; GitHub Flavored Markdown
  :custom
  (markdown-command "pandoc")  ; Use pandoc for preview/export
  (markdown-enable-math t)     ; Enable LaTeX math
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)  ; Syntax highlight code blocks
  (markdown-gfm-use-electric-backquote t)
  (markdown-indent-on-enter 'indent-and-new-item)

  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-export)
              ("C-c C-p" . markdown-preview)
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-i" . markdown-insert-image)))

;; ============================================================
;; Markdown TOC - Auto-generate table of contents
;; ============================================================
(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

;; ============================================================
;; Grip Mode - GitHub-style preview in browser
;; ============================================================
(use-package grip-mode
  :after markdown-mode
  :custom
  (grip-preview-use-webkit nil)  ; Use external browser
  :bind (:map markdown-mode-map
              ("C-c C-g" . grip-mode)))

;; ============================================================
;; Mermaid Mode - Diagrams
;; ============================================================
(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :custom
  (mermaid-mmdc-location (executable-find "mmdc"))
  :config
  ;; Syntax highlight mermaid blocks in markdown
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode)))

;; ============================================================
;; Edit code blocks in separate buffer
;; ============================================================
(use-package edit-indirect
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c '" . markdown-edit-code-block)))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
