;;; lyzell-selection.el -*- lexical-binding: t; -*-

;; Install dependencies.
(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'embark)

;; Setup vertico.
(require 'vertico)
(require 'vertico-directory "extensions/vertico-directory.el")

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "C-\d") 'vertico-directory-up)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous))
(setq vertico-cycle t)
(vertico-mode 1)

;; Setup marginalia.
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Setup orderless.
(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (style . (partial-completion)))))

;; Setup savehist.
(require 'savehist)
(savehist-mode)

;; Setup embark.
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-c") 'embark-act)
(setq prefix-help-command #'embark-prefix-help-command)

;; Setup consult.
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)
(setq completion-in-region-function #'consult-completion-in-region)

(provide 'lyzell-selection)
