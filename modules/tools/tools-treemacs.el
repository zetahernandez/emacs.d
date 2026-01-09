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

;; ============================================================
;; Treemacs + project.el integration
;; ============================================================
;; Sync treemacs when switching projects
(defun zeta/treemacs-sync-project ()
  "Sync treemacs to show current project if treemacs is visible."
  (when (and (featurep 'treemacs)
             (fboundp 'treemacs-get-local-window)
             (fboundp 'treemacs-do-add-project-to-workspace))
    (when-let* ((treemacs-window (treemacs-get-local-window))
                (project (project-current))
                (root (expand-file-name (project-root project)))
                (name (file-name-nondirectory (directory-file-name root))))
      ;; Clear current workspace and add only the current project
      (treemacs-block
       (treemacs-do-remove-project-from-workspace
        (treemacs-project-at-point) t t)
       (treemacs-do-add-project-to-workspace root name)))))

;; Alternative: Use treemacs-add-and-display-current-project-exclusively
;; which handles the project detection from the correct buffer
(defun zeta/treemacs-display-project ()
  "Display current project exclusively in treemacs."
  (when (and (featurep 'treemacs)
             buffer-file-name)
    (let ((project (project-current)))
      (when project
        (treemacs-add-and-display-current-project-exclusively)))))

;; ============================================================
;; Auto-sync treemacs with current project
;; ============================================================
(with-eval-after-load 'treemacs
  (defvar zeta/treemacs-last-project nil
    "Last project shown in treemacs.")

  (defvar zeta/treemacs-last-file-buffer nil
    "Last buffer visiting a file.")

  (defvar zeta/treemacs-sync-timer nil
    "Timer for debouncing treemacs sync.")

  (defun zeta/treemacs-do-sync ()
    "Actually sync treemacs to current project."
    (when (and zeta/treemacs-last-file-buffer
               (buffer-live-p zeta/treemacs-last-file-buffer)
               (treemacs-get-local-window))
      (let ((current-window (selected-window)))
        (with-current-buffer zeta/treemacs-last-file-buffer
          (let* ((project (project-current))
                 (root (when project (expand-file-name (project-root project)))))
            (when (and root (not (equal root zeta/treemacs-last-project)))
              (setq zeta/treemacs-last-project root)
              (treemacs-add-and-display-current-project-exclusively)
              ;; Restore focus to original window
              (when (window-live-p current-window)
                (select-window current-window))))))))

  (defun zeta/treemacs-track-buffer ()
    "Track current buffer if it's visiting a file."
    (when buffer-file-name
      (setq zeta/treemacs-last-file-buffer (current-buffer))))

  (defun zeta/treemacs-maybe-sync ()
    "Debounced sync of treemacs to current project."
    ;; Track file buffers
    (zeta/treemacs-track-buffer)
    ;; Only sync if treemacs is visible and we have a file buffer
    (when (and zeta/treemacs-last-file-buffer
               (treemacs-get-local-window))
      ;; Cancel pending timer
      (when zeta/treemacs-sync-timer
        (cancel-timer zeta/treemacs-sync-timer))
      ;; Schedule new sync with debounce
      (setq zeta/treemacs-sync-timer
            (run-at-time 0.2 nil #'zeta/treemacs-do-sync))))

  ;; Sync when buffer changes in a window (Emacs 27+)
  (add-hook 'window-buffer-change-functions
            (lambda (_frame)
              (zeta/treemacs-maybe-sync)))

  ;; Also track on file open
  (add-hook 'find-file-hook #'zeta/treemacs-track-buffer)

  ;; Sync when opening treemacs - use buffer before treemacs opens
  (defvar zeta/treemacs-return-window nil
    "Window to return to after opening treemacs.")

  (advice-add 'treemacs :before
              (lambda (&rest _)
                (zeta/treemacs-track-buffer)
                (setq zeta/treemacs-return-window (selected-window))))

  (advice-add 'treemacs :after
              (lambda (&rest _)
                (setq zeta/treemacs-last-project nil)  ; Force sync
                (run-at-time 0.1 nil
                             (lambda ()
                               (zeta/treemacs-do-sync)
                               ;; Return focus to original window
                               (when (and zeta/treemacs-return-window
                                          (window-live-p zeta/treemacs-return-window))
                                 (select-window zeta/treemacs-return-window))))))

  ;; Sync after project switch
  (advice-add 'project-switch-project :after
              (lambda (&rest _)
                (setq zeta/treemacs-last-project nil)
                (run-at-time 0.3 nil #'zeta/treemacs-do-sync))))

(provide 'tools-treemacs)
;;; tools-treemacs.el ends here
