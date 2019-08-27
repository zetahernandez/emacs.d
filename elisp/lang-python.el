;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

;; (use-package python
;;   :mode ("\\.py" . python-mode)
;;   :config
;;   (use-package elpy
;;     :init
;;     (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;     :config
;;     (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;;     (setq elpy-rpc-backend "jedi")
;;     (setq elpy-rpc-python-command "python3")
;;     (setq python-shell-interpreter "python3")

;;     (setq python-shell-interpreter "ipython3"
;;       python-shell-interpreter-args "-i")
;;     ;; (add-hook 'python-mode-hook #'lsp)
;;     ;; use flymake on emacs 26.1
;;     ;; use flycheck not flymake with elpy
;;     ;; (when (require 'flycheck nil t)
;;     ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     ;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;;     ;; (setq python-shell-interpreter "jupyter"
;;     ;;       python-shell-interpreter-args "console --simple-prompt"
;;     ;;       python-shell-prompt-detect-failure-warning nil)
;;     ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;     ;;          "jupyter")
;;     ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;     ;;flycheck-python-flake8-executable "/usr/local/bin/flake8"
;;     :bind (:map elpy-mode-map
;; 	      ("M-." . elpy-goto-definition)
;; 	      ("M-," . pop-tag-mark)))
;;   (elpy-enable))

;; (use-package py-autopep8)

(use-package hydra)

(use-package helm-lsp
  :config
  (defun netrom/helm-lsp-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-workspace-symbol)))

  (defun netrom/helm-lsp-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-global-workspace-symbol))))

(use-package lsp-mode
  :ensure t
  :config

  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-mode)
  (require 'lsp-clients)
  (require 'hydra)
  (require 'helm)
  (require 'helm-lsp)

  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-pyls-plugins-pylint-enabled nil)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq-default lsp-pyls-configuration-sources ["flake8"])

  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; get lsp-python-enable defined
  ;; NB: use either projectile-project-root or ffip-get-project-root-directory
  ;;     or any other function that can be used to find the root directory of a project
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :server-id 'pyls))
  ;; make sure this is activated when python-mode is activated
  ;; lsp-python-enable is created by macro above
  (add-hook 'python-mode-hook #'lsp)


  (use-package lsp-ui
    :custom-face
    (lsp-ui-doc-background ((t (:background nil))))
    (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
    :requires lsp-mode flycheck
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references)
                ("C-c u" . lsp-ui-imenu))

    :config
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25)

    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (setq netrom--general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
          ("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" helm-imenu "Filter funcs/classes (Helm)")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        netrom--misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
  (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
           ,@(append
              netrom--general-lsp-hydra-heads
              netrom--misc-lsp-hydra-heads)))

  (add-hook 'lsp-mode-hook
            (lambda () (local-set-key (kbd "C-c C-l") 'netrom/lsp-hydra/body)))

  (use-package company
    :config
    (setq company-idle-delay 0.3)

    (global-company-mode 1)

    (global-set-key (kbd "C-<tab>") 'company-complete))

  (use-package company-lsp
    :requires company
    :config
    (push 'company-lsp company-backends)

    ;; Disable client-side cache because the LSP server does a better job.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)))

(use-package dap-mode
  :defer    t
  :config
  (setq dap-python-executable "python3")
  ;; (setq dap-python-default-debug-port 5678)
  (require 'dap-python))

(provide 'lang-python)
;;; base-python.el ends here
