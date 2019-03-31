(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
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
   undo-tree-auto-save-history nil
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

(provide 'base-extension)
