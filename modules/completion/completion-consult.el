;; -*- lexical-binding: t; -*-
;;; completion-consult.el --- Consult commands + Embark actions -*- lexical-binding: t -*-

;;; Commentary:
;; Consult provides search and navigation commands
;; Embark provides contextual actions on candidates
;; Together they replace Helm's search functionality

;;; Code:

;; ============================================================
;; Consult - Search and navigation commands
;; ============================================================
(use-package consult
  :bind
  (;; C-c bindings (mode-specific)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)
   ("C-x b"   . consult-buffer)              ; was switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)

   ;; M-g bindings (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g f"   . consult-flymake)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)

   ;; M-s bindings (search-map)
   ("M-s d"   . consult-find)                ; find files
   ("M-s g"   . consult-grep)                ; grep
   ("M-s G"   . consult-git-grep)            ; git grep
   ("M-s r"   . consult-ripgrep)             ; ripgrep (recommended)
   ("M-s l"   . consult-line)                ; search in buffer
   ("M-s L"   . consult-line-multi)          ; search in all buffers
   ("M-s k"   . consult-keep-lines)
   ("M-s u"   . consult-focus-lines)

   ;; Custom bindings (reemplazos de Helm)
   ("C-s"     . consult-line)                ; was helm-occur / isearch
   ("C-r"     . consult-line)                ; was helm-occur
   ("C-x f"   . consult-recent-file)         ; was helm-recentf
   ("M-y"     . consult-yank-pop)            ; was helm-show-kill-ring

   ;; Isearch integration
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))

  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Use Consult for xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Configure preview
  (setq consult-preview-key "M-.")

  ;; Narrowing key
  (setq consult-narrow-key "<")

  ;; Project root detection (uses project.el)
  (setq consult-project-function #'consult--default-project-function))

;; ============================================================
;; Embark - Contextual actions
;; ============================================================
(use-package embark
  :bind
  (("C-."   . embark-act)         ; Primary action key
   ("C-;"   . embark-dwim)        ; Smart default action
   ("C-h B" . embark-bindings))   ; Show available bindings

  :init
  ;; Use Embark for prefix-help
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide modeline in Embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; ============================================================
;; Embark-Consult integration
;; ============================================================
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completion-consult)
;;; completion-consult.el ends here
