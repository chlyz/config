;;; lyzell-programming.el -*- lexical-binding; t: -*-

;;;; Install dependencies.
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package '(matlab-emacs
                        :type git
                        :repo "https://git.code.sf.net/p/matlab-emacs/src"))

;;;; Indent by spaces
(setq-default indent-tabs-mode nil)

;; Whitespace
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook
                ))
  (add-hook mode (lambda () (setq show-trailing-whitespace 't))))

;;;; C/C++
(setq-default c-default-style "linux")	; general style
(setq-default c-basic-offset 4)		; indent by four spaces
(setq c-default-style "bsd")

;;;; Octave
(setq-default octave-block-offset 4)	; indent by four spaces
(setq octave-comment-char 37)	        ; comment with %
(setq octave-comment-start "% ")        ; comment with %
(setq octave-block-comment-start "%% ") ; comment with %

;;;; Matlab
(require 'matlab)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(modify-syntax-entry ?_ "w" matlab-mode-syntax-table)
(customize-set-value 'matlab-align-to-paren nil)

;;;; Bazel
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

;;;; SCons
(add-to-list 'auto-mode-alist '("\\SConscript*\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct*\\'" . python-mode))

;;;; Line numbers

(defun lyzell/toggle-relative-line-numbers ()
  "Toggle between absolute and relative line numbers."
  (interactive)
  (if (eq display-line-numbers-type 'visual)
      (setq display-line-numbers-type 't)
    (setq display-line-numbers-type 'visual))
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode 1)))

(defun lyzell/dynamic-line-numbers-width ()
  "Compute the line number width."
  (setq display-line-numbers-width
	(length (number-to-string (line-number-at-pos (point-max))))))

(column-number-mode 1)
(setq-default display-line-numbers-grow-only t)
(setq-default display-line-numbers-width 2)
(setq-default display-line-numbers-type 'visual)            ; default to relative line numbers
; (add-hook 'find-file-hook 'lyzell/dynamic-line-numbers-width) ; compute the line number width when changing the file
(dolist (mode '(text-mode-hook                                ; enable line numbers for certain modes
                prog-mode-hook
                conf-mode-hook
                matlab-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;;;; Setup commenting.
(require 'evil-nerd-commenter)
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)

;;;; Setup treesitter highlightning.
; (require 'tree-sitter)
; (require 'tree-sitter-langs)
; (global-tree-sitter-mode)
; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'lyzell-programming)
