;; -*- lexical-binding: t; -*-
;;; lang-python.el --- Python configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Python development with pyright + ruff
;; Supports per-project virtualenvs (uv, poetry, etc.)

;;; Code:

;; ============================================================
;; Pet - Virtualenv detection for uv/poetry/pipenv/etc.
;; ============================================================
(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; ============================================================
;; LSP Pyright - Type checking and completions
;; ============================================================
(use-package lsp-pyright
  :ensure t
  :custom
  ;; Use the Python from the virtualenv (detected by pet)
  (lsp-pyright-python-executable-cmd "python3")

  ;; Type checking mode: basic, standard, strict
  (lsp-pyright-typechecking-mode "basic")

  ;; Auto-detect venv
  (lsp-pyright-venv-path nil)  ; Let pet handle this

  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred)))
  (python-ts-mode . (lambda ()
                      (require 'lsp-pyright)
                      (lsp-deferred))))

;; ============================================================
;; Ruff LSP - Linting and formatting (per-project)
;; ============================================================
(with-eval-after-load 'lsp-mode
  ;; Function to find ruff from project's venv or fallback to global
  (defun zeta/ruff-server-command ()
    "Return the ruff server command, preferring project-local version."
    (let ((local-ruff (when (bound-and-true-p pet-mode)
                        (pet-executable-find "ruff"))))
      (if local-ruff
          (list local-ruff "server")
        '("ruff" "server"))))

  ;; Register ruff as a language server
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'zeta/ruff-server-command)
    :activation-fn (lsp-activate-on "python")
    :add-on? t  ; Run alongside pyright
    :server-id 'ruff-lsp
    :priority -1  ; Lower priority than pyright
    :initialization-options
    '(:settings
      (:lineLength 88
       :organizeImports t
       :fixAll t)))))

;; ============================================================
;; Python mode settings
;; ============================================================
(use-package python
  :ensure nil  ; built-in
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil)

  :config
  ;; Format with ruff (uses project-local version when available)
  (defun zeta/python-format-buffer ()
    "Format Python buffer with ruff, preferring project-local version."
    (interactive)
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (let* ((ruff-cmd (or (when (bound-and-true-p pet-mode)
                             (pet-executable-find "ruff"))
                           (executable-find "ruff")))
             (point (point)))
        (when ruff-cmd
          (shell-command-on-region
           (point-min) (point-max)
           (format "%s format --stdin-filename=buffer.py -" ruff-cmd)
           (current-buffer) t)
          (goto-char point)))))

  ;; Optional: auto-format on save (uncomment if desired)
  ;; (add-hook 'before-save-hook #'zeta/python-format-buffer)
  )

;; ============================================================
;; Tree-sitter for Python (better syntax highlighting)
;; ============================================================
(use-package python-ts-mode
  :ensure nil  ; built-in in Emacs 29+
  :mode ("\\.py\\'" . python-ts-mode)
  :when (treesit-available-p))

;; ============================================================
;; Keybindings for Python
;; ============================================================
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-f") #'zeta/python-format-buffer)
  (define-key python-mode-map (kbd "C-c C-p") #'run-python))

;; ============================================================
;; Pet integration with LSP
;; ============================================================
(defun zeta/pet-update-lsp-pyright ()
  "Update lsp-pyright to use pet-detected Python."
  (when (and (bound-and-true-p pet-mode)
             (pet-executable-find "python"))
    (setq-local lsp-pyright-python-executable-cmd
                (pet-executable-find "python"))
    (setq-local lsp-pyright-venv-path
                (pet-virtualenv-root))))

(add-hook 'python-base-mode-hook #'zeta/pet-update-lsp-pyright)

(provide 'lang-python)
;;; lang-python.el ends here
