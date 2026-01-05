;; -*- lexical-binding: t; -*-
;;; core-packages.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:
;; Configure package repositories and use-package

;;; Code:

;; Configurar repositorios
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities
      '(("melpa"  . 10)
        ("gnu"    . 5)
        ("nongnu" . 1)))

;; Inicializar package.el
(package-initialize)

;; Refrescar contenido si es necesario
(unless package-archive-contents
  (package-refresh-contents))

;; use-package está built-in en Emacs 29+
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'core-packages)
;;; core-packages.el ends here
