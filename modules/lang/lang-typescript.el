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
  :ensure t
  :config
  (defun zeta/nvm-version-major (version-string)
    "Extract major version number from VERSION-STRING like '12.22.9' or 'v18.20.8'."
    (when (string-match "v?\\([0-9]+\\)" version-string)
      (string-to-number (match-string 1 version-string))))

  (defun zeta/nvm-use-for-buffer ()
    "Activate Node version from .nvmrc if present and >= 20 (for LSP compatibility)."
    (when buffer-file-name
      (let ((nvmrc-dir (locate-dominating-file buffer-file-name ".nvmrc")))
        (when nvmrc-dir
          (let* ((nvmrc-file (expand-file-name ".nvmrc" nvmrc-dir))
                 (version (string-trim (with-temp-buffer
                                         (insert-file-contents nvmrc-file)
                                         (buffer-string))))
                 (major (zeta/nvm-version-major version)))
            (if (and major (>= major 20))
                (condition-case err
                    (nvm-use-for buffer-file-name)
                  (error (message "nvm: %s" (error-message-string err))))
              (message "nvm: skipping old Node %s (need >= 20 for LSP)" version)))))))

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
(with-eval-after-load 'lsp-mode
  ;; ESLint 9 flat config support
  (setq lsp-eslint-experimental-use-flat-config t)
  ;; Ensure eslint validates these file types
  (setq lsp-eslint-validate
        '("javascript" "javascriptreact" "typescript" "typescriptreact")))

;; ============================================================
;; Prettier formatting
;; ============================================================
(defun zeta/prettier-format-buffer ()
  "Format current buffer using Prettier from project's node_modules."
  (interactive)
  (let* ((file (buffer-file-name))
         (project-root (locate-dominating-file file "package.json"))
         (prettier-local (when project-root
                           (expand-file-name "node_modules/.bin/prettier" project-root)))
         (prettier-cmd (if (and prettier-local (file-executable-p prettier-local))
                           prettier-local
                         "prettier")))
    (if (not file)
        (message "Buffer is not visiting a file")
      (let ((point-pos (point))
            (window-start-pos (window-start)))
        (shell-command-on-region
         (point-min) (point-max)
         (format "%s --stdin-filepath %s" prettier-cmd (shell-quote-argument file))
         (current-buffer) t "*Prettier Errors*" t)
        (goto-char point-pos)
        (set-window-start (selected-window) window-start-pos)))))

;; ============================================================
;; Keybindings for TypeScript/JavaScript (via hooks)
;; ============================================================
(defun zeta/eslint-fix-buffer ()
  "Fix ESLint errors in current buffer using project's eslint."
  (interactive)
  (let* ((file (buffer-file-name))
         (project-root (locate-dominating-file file "package.json"))
         (eslint-local (when project-root
                         (expand-file-name "node_modules/.bin/eslint" project-root)))
         (eslint-cmd (if (and eslint-local (file-executable-p eslint-local))
                         eslint-local
                       "eslint")))
    (when file
      (save-buffer)
      (let ((output (shell-command-to-string
                     (format "%s --fix %s" eslint-cmd (shell-quote-argument file)))))
        (revert-buffer t t t)
        (message "ESLint fix applied")))))

(defun zeta/ts-js-keybindings ()
  "Set up keybindings for TypeScript/JavaScript modes."
  (local-set-key (kbd "C-c C-f") #'zeta/prettier-format-buffer)
  (local-set-key (kbd "C-c C-e") #'zeta/eslint-fix-buffer))

(add-hook 'typescript-ts-mode-hook #'zeta/ts-js-keybindings)
(add-hook 'tsx-ts-mode-hook #'zeta/ts-js-keybindings)
(add-hook 'js-ts-mode-hook #'zeta/ts-js-keybindings)

(provide 'lang-typescript)
;;; lang-typescript.el ends here
