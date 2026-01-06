;; -*- lexical-binding: t; -*-
;;; lang-typescript.el --- TypeScript and React configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TypeScript, TSX, and JavaScript support with tree-sitter and LSP
;; Uses nvm.el for per-project Node version detection

;;; Code:

;; ============================================================
;; Tree-sitter modes for TypeScript/TSX
;; ============================================================
(use-package typescript-ts-mode
  :ensure nil  ; built-in in Emacs 29+
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode))
  :when (treesit-available-p))

(use-package tsx-ts-mode
  :ensure nil  ; built-in in Emacs 29+
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :when (treesit-available-p))

;; JavaScript with tree-sitter
(use-package js-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode))
  :when (treesit-available-p))

;; JSON with tree-sitter
(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :when (treesit-available-p))

;; ============================================================
;; nvm.el - Per-project Node version management
;; ============================================================
(use-package nvm
  :config
  (defun zeta/nvm-use-for-buffer ()
    "Activate Node version from .nvmrc if present in project."
    (when buffer-file-name
      (let ((nvmrc (locate-dominating-file buffer-file-name ".nvmrc")))
        (when nvmrc
          (condition-case err
              (nvm-use-for buffer-file-name)
            (error (message "nvm: %s" (error-message-string err))))))))

  ;; Auto-activate correct Node version when opening TS/JS files
  (add-hook 'typescript-ts-mode-hook #'zeta/nvm-use-for-buffer)
  (add-hook 'tsx-ts-mode-hook #'zeta/nvm-use-for-buffer)
  (add-hook 'js-ts-mode-hook #'zeta/nvm-use-for-buffer)
  (add-hook 'json-ts-mode-hook #'zeta/nvm-use-for-buffer))

;; ============================================================
;; LSP for TypeScript/JavaScript
;; ============================================================
(with-eval-after-load 'lsp-mode
  ;; TypeScript/JavaScript LSP settings
  (setq lsp-typescript-suggest-complete-function-calls t)

  ;; Activate LSP for TS/JS modes
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
  (add-hook 'js-ts-mode-hook #'lsp-deferred))

;; ============================================================
;; ESLint via LSP
;; ============================================================
(use-package lsp-eslint
  :ensure nil  ; Part of lsp-mode
  :after lsp-mode
  :config
  ;; Auto-fix on save (optional, uncomment if desired)
  ;; (setq lsp-eslint-auto-fix-on-save t)
  )

;; ============================================================
;; Keybindings for TypeScript
;; ============================================================
(defun zeta/typescript-format-buffer ()
  "Format TypeScript buffer using LSP."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-format-buffer)
    (message "LSP not active")))

(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "C-c C-f") #'zeta/typescript-format-buffer))

(with-eval-after-load 'tsx-ts-mode
  ;; tsx-ts-mode inherits from typescript-ts-mode, but just in case
  (when (boundp 'tsx-ts-mode-map)
    (define-key tsx-ts-mode-map (kbd "C-c C-f") #'zeta/typescript-format-buffer)))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
