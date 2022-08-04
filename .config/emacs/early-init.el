;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Increase the garbage collection threshold for faster startup.
(setq gc-cons-threshold (* 50 1000 1000))


(setq modus-themes-italic-constructs t)
(setq modus-themes-mode-line '(padded))
;; (setq modus-themes-syntax '(yellow-comments))
(setq modus-themes-syntax nil)
(load-theme 'modus-vivendi t nil)
;; Prefer loading newest compiled .el file.
(customize-set-variable 'load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)     ; silence compiler warnings
  (setq package-native-compile t)		          ; native compilation of packages
  (setq native-comp-deferred-compilation t)               ; asynchroneous compilation
  (setq native-comp-async-report-warnings-errors 'silent) ; report warning and errors silently
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Do not initialize package.el, use straight instead.
(setq package-enable-at-startup nil)

;; Allow loading from the package cache
;; (defvar package-quickstart)
;; (setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-buffer-menu t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(background-color . "#232635") default-frame-alist)
;; (push '(foreground-color . "#FFFFFF") default-frame-alist)
;; (push '(mouse-color . "white") default-frame-alist)
;; (scroll-bar-mode -1)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)		; only for mouse events

(setq initial-major-mode 'fundamental-mode)
