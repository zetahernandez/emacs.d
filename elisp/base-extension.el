(use-package all-the-icons)

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package better-defaults)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project)
         ("C-x v" . helm-projectile)
         ("C-s"     . helm-occur)
         ("C-r"     . helm-occur)
         ("C-x c p" . helm-projectile-ag)
         ("C-x c k" . helm-show-kill-ring)
         ("C-x f" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  ;; open helm buffer inside current window, don't occupy the entire other window
  (setq helm-split-window-in-side-p t)
  (setq helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-completion-in-region-fuzzy-match t))

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action (lambda (&optional arg) (helm-browse-project arg)))

  (projectile-global-mode))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq helm-projectile-fuzzy-match nil))

(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg --no-heading")
  (bind-key* "C-c p s r" 'helm-do-ag-project-root))

(use-package helm-ls-git)

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

(use-package flycheck)

(use-package magit
  :config

  (setq magit-completing-read-function 'ivy-completing-read)

  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g b" . magit-blame)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive)
  ("C-x g l" . magit-log))

(use-package magit-popup)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package smartparens)

(use-package smex)

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down)
  ("C-c <left>" . windmove-left)
  ("C-c <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker)

(use-package restclient)

(use-package ob-restclient)

(use-package ox-rst)

(use-package py-isort)

(use-package powerline
  :config
  (powerline-default-theme))

(use-package apib-mode
  :mode ("\\.apib\\'" . apib-mode))

(use-package beacon
  :defer 5
  :config (beacon-mode 1))

(use-package volatile-highlights
  :defer 10
  :config (volatile-highlights-mode t))

(use-package fill-column-indicator
  :hook ((emacs-lisp git-commit-setup) . fci-mode))

(use-package copy-as-format
  :bind (:map mode-specific-map
         :prefix-map copy-as-format-prefix-map
         :prefix "w"
         ("w" . copy-as-format)
         ("g" . copy-as-format-github)
         ("h" . copy-as-format-hipchat-pidgin)
         ("j" . copy-as-format-jira)
         ("m" . copy-as-format-markdown)
         ("o" . copy-as-format-org-mode)
         ("r" . copy-as-format-rst)
         ("s" . copy-as-format-slack)
         ("v" . org-copy-visible))
  :config
  ;; (setq copy-as-format-default "slack")
  ;; Define own format since pidgin doesn't allow to begin a message with `/code'
  (defun copy-as-format--hipchat-pidgin (text _multiline)
    (format "/say /code %s" text))
  (add-to-list 'copy-as-format-format-alist '("hipchat-pidgin" copy-as-format--hipchat-pidgin))
  (defun copy-as-format-hipchat-pidgin ()
    (interactive)
    (setq copy-as-format-default "hipchat-pidgin")
    (copy-as-format)))

(use-package move-text
  :bind (([(meta shift up)]      . move-text-up)
         ([(meta shift down)]    . move-text-down)))

(use-package restclient-helm
  :disabled t
  :after (restclient helm))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


(provide 'base-extension)
