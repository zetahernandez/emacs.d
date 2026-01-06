;; -*- lexical-binding: t; -*-
;;; tools-ai.el --- AI integration (gptel + copilot) -*- lexical-binding: t -*-

;;; Commentary:
;; AI copilot features similar to Cursor:
;; - gptel: Chat with Claude/Gemini (send code, ask questions, refactor)
;; - copilot.el: Inline code completion (Tab to accept)

;;; Code:

;; ============================================================
;; gptel - Multi-LLM chat client
;; ============================================================
(use-package gptel
  :vc (:url "https://github.com/karthink/gptel"
       :rev :newest
       :branch "master")
  :config
  ;; Claude (Anthropic) como backend principal - usa CLAUDE_API_KEY
  (setq gptel-model 'claude-opus-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (lambda ()
                               (or (getenv "CLAUDE_API_KEY")
                                   (getenv "ANTHROPIC_API_KEY")))))

  ;; Gemini (Google) como backend alternativo
  (gptel-make-gemini "Gemini"
    :stream t
    :key (lambda () (getenv "GEMINI_API_KEY")))

  ;; Configuracion de UI
  (setq gptel-default-mode 'org-mode)  ; Respuestas en org-mode

  :bind
  (("C-c a a" . gptel)                 ; Abrir/toggle chat buffer
   ("C-c a s" . gptel-send)            ; Enviar region o buffer
   ("C-c a m" . gptel-menu)            ; Menu transient con opciones
   ("C-c a r" . gptel-rewrite)         ; Reescribir region con IA
   ("C-c a b" . gptel-add)             ; Agregar contexto al chat
   ("C-c a k" . gptel-abort)))         ; Cancelar request

;; ============================================================
;; GitHub Copilot - Inline completion
;; ============================================================
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :hook ((prog-mode . copilot-mode)
         (yaml-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("C-g" . copilot-clear-overlay))
  :config
  ;; No mostrar sugerencias cuando hay menu de completion abierto
  (setq copilot-idle-delay 0.5)

  ;; Desactivar en ciertos modos
  (add-to-list 'copilot-disable-predicates
               (lambda () (bound-and-true-p corfu--frame)))

  ;; Asegurar que Tab funciona bien con otras herramientas
  (with-eval-after-load 'corfu
    ;; Si corfu esta activo, no usar copilot
    (define-key corfu-map (kbd "<tab>")
      (lambda ()
        (interactive)
        (if (copilot--overlay-visible)
            (copilot-accept-completion)
          (corfu-complete))))))

;; ============================================================
;; Comandos utiles adicionales
;; ============================================================
(defun zeta/ai-explain-region (start end)
  "Ask AI to explain the selected code."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (gptel-request
     (format "Explica este codigo de forma concisa:\n\n```\n%s\n```" code)
     :callback (lambda (response _)
                 (with-current-buffer (get-buffer-create "*AI Explanation*")
                   (erase-buffer)
                   (org-mode)
                   (insert response)
                   (goto-char (point-min))
                   (display-buffer (current-buffer)))))))

(defun zeta/ai-improve-region (start end)
  "Ask AI to suggest improvements for the selected code."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (gptel-request
     (format "Sugiere mejoras para este codigo (rendimiento, claridad, mejores practicas):\n\n```\n%s\n```" code)
     :callback (lambda (response _)
                 (with-current-buffer (get-buffer-create "*AI Suggestions*")
                   (erase-buffer)
                   (org-mode)
                   (insert response)
                   (goto-char (point-min))
                   (display-buffer (current-buffer)))))))

(defun zeta/ai-generate-docstring ()
  "Generate docstring for function at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'defun))
         (code (when bounds
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (if code
        (gptel-request
         (format "Genera un docstring conciso para esta funcion (formato del lenguaje apropiado):\n\n```\n%s\n```" code)
         :callback (lambda (response _)
                     (message "Docstring: %s" response)))
      (message "No function at point"))))

;; Keybindings adicionales
(global-set-key (kbd "C-c a e") #'zeta/ai-explain-region)
(global-set-key (kbd "C-c a i") #'zeta/ai-improve-region)
(global-set-key (kbd "C-c a d") #'zeta/ai-generate-docstring)

(provide 'tools-ai)
;;; tools-ai.el ends here
