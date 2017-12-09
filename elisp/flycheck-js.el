(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

  (defun eslint-fix ()
    "Format the current file with ESLint."
    (interactive)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (progn (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
               (revert-buffer t t t))))))

(provide 'flycheck-js)
