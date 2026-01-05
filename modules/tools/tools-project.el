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
  (("C-x p f" . project-find-file)
   ("C-x p p" . project-switch-project)
   ("C-x p b" . project-switch-to-buffer)
   ("C-x p d" . project-find-dir)
   ("C-x p g" . project-find-regexp)
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

  ;; Agregar proyectos conocidos
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-find-dir "Find dir" ?d)
          (project-vc-dir "VC dir" ?v)
          (project-shell "Shell" ?s)
          (magit-project-status "Magit" ?m))))

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

(provide 'tools-project)
;;; tools-project.el ends here
