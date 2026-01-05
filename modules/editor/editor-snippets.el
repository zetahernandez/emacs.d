;; -*- lexical-binding: t; -*-
;;; editor-snippets.el --- Yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Snippet expansion with yasnippet

;;; Code:

;; ============================================================
;; Yasnippet
;; ============================================================
(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;; ============================================================
;; Yasnippet snippets collection
;; ============================================================
(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================
;; Consult-yasnippet (integración con consult)
;; ============================================================
(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind ("C-c y" . consult-yasnippet))

(provide 'editor-snippets)
;;; editor-snippets.el ends here
