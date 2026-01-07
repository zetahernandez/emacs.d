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
              ("C-c C-i" . markdown-insert-image)
              ;; Liberar C-c <arrow> para window navigation
              ("C-c <up>" . nil)
              ("C-c <down>" . nil)
              ("C-c <left>" . nil)
              ("C-c <right>" . nil)))

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
  (mermaid-output-format ".png")
  :bind (:map mermaid-mode-map
              ("C-c C-c" . mermaid-compile)        ; compile file
              ("C-c C-b" . mermaid-compile-buffer) ; compile buffer
              ("C-c C-r" . mermaid-compile-region) ; compile region
              ("C-c C-o" . mermaid-open-browser)   ; open live editor
              ("C-c C-d" . mermaid-open-doc))      ; open docs
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

;; ============================================================
;; Markdown preview with Mermaid support
;; ============================================================
(defun zeta/markdown-preview-with-mermaid ()
  "Preview markdown in browser with mermaid diagram support."
  (interactive)
  (let* ((input-file (buffer-file-name))
         (output-file (concat (file-name-sans-extension input-file) "-preview.html"))
         (mermaid-script "<script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>
<script>mermaid.initialize({startOnLoad:true});</script>")
         (style "<style>
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; max-width: 900px; margin: 40px auto; padding: 0 20px; line-height: 1.6; }
pre { background: #f6f8fa; padding: 16px; border-radius: 6px; overflow-x: auto; }
code { background: #f6f8fa; padding: 2px 6px; border-radius: 3px; }
pre code { background: none; padding: 0; }
.mermaid { text-align: center; background: white; padding: 20px; }
</style>"))
    (save-buffer)
    (shell-command
     (format "pandoc '%s' -o '%s' --standalone --metadata title='Preview'"
             input-file output-file))
    ;; Inject mermaid.js and convert code blocks
    (with-temp-file output-file
      (insert-file-contents output-file)
      (goto-char (point-min))
      ;; Add style and mermaid script in head
      (when (search-forward "</head>" nil t)
        (replace-match (concat style "\n" mermaid-script "\n</head>")))
      ;; Convert mermaid code blocks: <pre class="mermaid"><code>...</code></pre>
      (goto-char (point-min))
      (while (re-search-forward "<pre class=\"mermaid\"><code>\\(\\(?:.\\|\n\\)*?\\)</code></pre>" nil t)
        (let ((content (match-string 1)))
          ;; Unescape HTML entities
          (setq content (replace-regexp-in-string "&gt;" ">" content))
          (setq content (replace-regexp-in-string "&lt;" "<" content))
          (setq content (replace-regexp-in-string "&amp;" "&" content))
          (setq content (replace-regexp-in-string "&quot;" "\"" content))
          (replace-match (concat "<div class=\"mermaid\">\n" content "\n</div>") t t))))
    (browse-url (concat "file://" output-file))
    (message "Preview opened in browser")))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-m") #'zeta/markdown-preview-with-mermaid)
  (define-key gfm-mode-map (kbd "C-c C-m") #'zeta/markdown-preview-with-mermaid))

;; ============================================================
;; LSP for Markdown (marksman)
;; ============================================================
(with-eval-after-load 'lsp-mode
  (add-hook 'markdown-mode-hook #'lsp-deferred)
  (add-hook 'gfm-mode-hook #'lsp-deferred))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
