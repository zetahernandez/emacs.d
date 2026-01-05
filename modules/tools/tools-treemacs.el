;; -*- lexical-binding: t; -*-
;;; tools-treemacs.el --- File tree sidebar -*- lexical-binding: t -*-

;;; Commentary:
;; Treemacs file explorer

;;; Code:

;; ============================================================
;; Treemacs
;; ============================================================
(use-package treemacs
  :defer t
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t t"   . treemacs)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t d"   . treemacs-select-directory)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-f" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-collapse-dirs              3
        treemacs-deferred-git-apply-delay   0.5
        treemacs-display-in-side-window     t
        treemacs-eldoc-display              t
        treemacs-file-event-delay           2000
        treemacs-file-follow-delay          0.2
        treemacs-follow-after-init          t
        treemacs-git-command-pipe           ""
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-max-git-entries            5000
        treemacs-missing-project-action     'ask
        treemacs-no-png-images              nil
        treemacs-no-delete-other-windows    t
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                   'left
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-cursor                nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-asc
        treemacs-width                      35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;; Git mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))

;; ============================================================
;; Treemacs + Magit integration
;; ============================================================
(use-package treemacs-magit
  :after (treemacs magit))

;; ============================================================
;; Treemacs icons in dired
;; ============================================================
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; ============================================================
;; Treemacs + nerd-icons (alternativa a all-the-icons)
;; ============================================================
(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'tools-treemacs)
;;; tools-treemacs.el ends here
