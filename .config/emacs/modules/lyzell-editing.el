;;; lyzell-editing.el -*- lexical-binding; t: -*-

;; Set the text width.
(setq-default truncate-lines 't)
(setq-default fill-column 80)
(setq fill-column 80)

;; Remember the cursor position.
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file "saveplace"))
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(provide 'lyzell-editing)
