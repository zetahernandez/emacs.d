;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :config
    (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python3")
    (setq python-shell-interpreter "python3")

    (setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")

    ;; use flymake on emacs 26.1
    ;; use flycheck not flymake with elpy
    ;; (when (require 'flycheck nil t)
    ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    ;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
    ;; (setq python-shell-interpreter "jupyter"
    ;;       python-shell-interpreter-args "console --simple-prompt"
    ;;       python-shell-prompt-detect-failure-warning nil)
    ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
    ;;          "jupyter")
    ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    ;;flycheck-python-flake8-executable "/usr/local/bin/flake8"
    :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark)))
  (elpy-enable))

(use-package py-autopep8)

(provide 'lang-python)
;;; base-python.el ends here
