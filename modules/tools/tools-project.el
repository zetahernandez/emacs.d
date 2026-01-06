;; -*- lexical-binding: t; -*-
;;; tools-project.el --- Project management -*- lexical-binding: t -*-

;;; Commentary:
;; project.el configuration (built-in, replaces Projectile)

;;; Code:

;; ============================================================
;; Project.el (built-in)
;; ============================================================
(use-package project
  :ensure nil  ; built-in
  :bind
  (;; C-x p f and C-x p g replaced by fzf versions below
   ("C-x p p" . project-switch-project)
   ("C-x p b" . project-switch-to-buffer)
   ("C-x p d" . project-find-dir)
   ("C-x p r" . project-query-replace-regexp)
   ("C-x p c" . project-compile)
   ("C-x p s" . project-shell)
   ("C-x p e" . project-eshell)
   ("C-x p k" . project-kill-buffers)
   ("C-x p !" . project-shell-command)
   ("C-x p &" . project-async-shell-command))
  :config
  ;; Usar fd si está disponible (más rápido que find)
  (when (executable-find "fd")
    (setq project-find-functions '(project-try-vc)))

  ;; project-switch-commands se configura después de cargar fzf
  )

;; ============================================================
;; Auto-discover projects in ~/dev
;; ============================================================
(defvar zeta/project-dirs '("~/dev")
  "Directories to scan for projects.")

(defun zeta/discover-projects ()
  "Scan project directories and remember all projects."
  (interactive)
  (require 'project)
  (dolist (dir zeta/project-dirs)
    (let ((expanded-dir (expand-file-name dir)))
      (when (file-directory-p expanded-dir)
        (condition-case err
            (project-remember-projects-under expanded-dir t)
          (error (message "Error scanning %s: %s" dir err))))))
  (message "Projects discovered!"))

;; Descubrir proyectos después de iniciar Emacs
(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (and (boundp 'project--list) project--list)
              (zeta/discover-projects))))

;; ============================================================
;; Consult integration with project.el
;; ============================================================
;; Ya configurado en completion-consult.el:
;; - C-x p b -> consult-project-buffer
;; - M-s r   -> consult-ripgrep (respeta proyecto)

;; ============================================================
;; fzf.el - Fuzzy finder integration
;; ============================================================
(use-package fzf
  :demand t
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/position-bottom t
        fzf/window-height 15)

  ;; Use ripgrep for text search
  (setq fzf/grep-command "rg --no-heading --line-number --color=never")

  ;; Use fdfind for file finding (respects .gitignore)
  (setq fzf/directory-start-command "fdfind --type f --follow --exclude .git"))

;; Project-aware fzf commands (defined after fzf loads)
(defun zeta/fzf-project-find-file ()
  "Find file in current project using fzf + fdfind."
  (interactive)
  (require 'fzf)
  (let ((project (project-current)))
    (if project
        (let ((default-directory (project-root project)))
          (fzf-with-command
           "fdfind --type f --follow --exclude .git"
           (lambda (x)
             (let ((f (expand-file-name x default-directory)))
               (when (file-exists-p f)
                 (find-file f))))
           default-directory))
      (user-error "Not in a project"))))

(defun zeta/fzf-rg-action (result)
  "Open file from ripgrep RESULT (format: file:line:col:text)."
  (when (and result (not (string-empty-p result)))
    ;; Remove ANSI color codes
    (let* ((clean (ansi-color-filter-apply result))
           (parts (split-string clean ":"))
           (file (car parts))
           (line (string-to-number (or (nth 1 parts) "1"))))
      (when (and file (file-exists-p file))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun zeta/fzf-project-grep ()
  "Live ripgrep search in current project using fzf."
  (interactive)
  (require 'fzf)
  (let ((project (project-current)))
    (if project
        (let* ((default-directory (project-root project))
               (rg-base "rg --column --line-number --no-heading --color=always --smart-case -- ")
               ;; Override fzf args for live reload mode
               (fzf/args (concat "--ansi --disabled "
                                 "--bind \"start:reload:" rg-base " {q} || true\" "
                                 "--bind \"change:reload:sleep 0.05; " rg-base " {q} || true\" "
                                 "--delimiter : "
                                 "--print-query --margin=1,0 --no-hscroll")))
          (fzf--start default-directory #'zeta/fzf-rg-action))
      (user-error "Not in a project"))))

;; Bind to project-prefix-map
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "f") #'zeta/fzf-project-find-file)
  (define-key project-prefix-map (kbd "g") #'zeta/fzf-project-grep)

  ;; Update project-switch-commands to use fzf
  (setq project-switch-commands
        '((zeta/fzf-project-find-file "Find file (fzf)" ?f)
          (zeta/fzf-project-grep "Find text (rg+fzf)" ?g)
          (project-find-dir "Find dir" ?d)
          (project-vc-dir "VC dir" ?v)
          (project-shell "Shell" ?s)
          (magit-project-status "Magit" ?m))))

;; Global fzf bindings
(defun zeta/fzf-find-file ()
  "Find file in current directory using fzf + fdfind."
  (interactive)
  (require 'fzf)
  (fzf-with-command
   "fdfind --type f --follow --exclude .git"
   (lambda (x)
     (let ((f (expand-file-name x default-directory)))
       (when (file-exists-p f)
         (find-file f))))
   default-directory))
(global-set-key (kbd "C-c z") #'zeta/fzf-find-file)
;; C-c Z uses same live rg but in current directory
(defun zeta/fzf-rg-live ()
  "Live ripgrep search in current directory."
  (interactive)
  (require 'fzf)
  (let* ((rg-base "rg --column --line-number --no-heading --color=always --smart-case -- ")
         (fzf/args (concat "--ansi --disabled "
                           "--bind \"start:reload:" rg-base " {q} || true\" "
                           "--bind \"change:reload:sleep 0.05; " rg-base " {q} || true\" "
                           "--delimiter : "
                           "--print-query --margin=1,0 --no-hscroll")))
    (fzf--start default-directory #'zeta/fzf-rg-action)))
(global-set-key (kbd "C-c Z") #'zeta/fzf-rg-live)

(provide 'tools-project)
;;; tools-project.el ends here
