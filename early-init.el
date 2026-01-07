;; -*- lexical-binding: t; -*-
;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Reducir GC durante startup (restaurar después en init.el)
(setq gc-cons-threshold most-positive-fixnum)

;; Deshabilitar package.el al inicio (lo inicializamos manualmente)
(setq package-enable-at-startup nil)

;; No redimensionar frame al cargar fuentes
(setq frame-inhibit-implied-resize t)

;; Deshabilitar UI elements antes de que carguen (más rápido que después)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Evitar flash blanco al iniciar
(push '(background-color . "#1e1e1e") default-frame-alist)

;; Maximizar ventana al iniciar
(push '(fullscreen . maximized) default-frame-alist)

;; No mostrar mensaje de startup
(setq inhibit-startup-message t)

;;; early-init.el ends here
