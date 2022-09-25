;;; lyzell-programming.el -*- lexical-binding; t: -*-

;;; Install dependencies.
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'jenkinsfile-mode)
(straight-use-package '(matlab-emacs
                        :type git
                        :repo "https://git.code.sf.net/p/matlab-emacs/src"))
(straight-use-package '(global-tags
                        :type git
                        :repo "https://github.com/emacsmirror/global-tags.git"))
(straight-use-package '(emacs-bazel-mode
                        :type git
                        :repo "https://github.com/bazelbuild/emacs-bazel-mode.git"))

(require 'jenkinsfile-mode)
;; (require 'bazel-mode)
;; (require 'bazel-build-mode)
;; (require 'bazel-workspace-mode)
;; (require 'bazelrc-mode)
;; (require 'bazelignore-mode)
;; (require 'bazel-starlark-mode)

(add-hook 'c-mode-hook #'global-tags-exclusive-backend-mode)

;;; Indent by spaces
(setq-default indent-tabs-mode nil)

;; (global-hl-line-mode 1)

;; Whitespace
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook
                matlab-mode-hook
                ))
  (add-hook mode (lambda ()
                   (setq show-trailing-whitespace t))))

(add-hook 'conf-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'proced-mode-hook #'hl-line-mode)
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
(add-hook 'grep-hook (lambda () (switch-to-buffer-other-window "*grep*")))

;;; C/C++
(setq-default c-default-style "linux")	; general style
(setq-default c-basic-offset 4)		; indent by four spaces
(setq c-default-style "bsd")

;;; Octave
(setq-default octave-block-offset 4)	; indent by four spaces
(setq octave-comment-char 37)	        ; comment with %
(setq octave-comment-start "% ")        ; comment with %
(setq octave-block-comment-start "%% ") ; comment with %

;;; Matlab

;; (require 'matlab)
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
;; (modify-syntax-entry ?_ "w" matlab-mode-syntax-table)
;; (customize-set-value 'matlab-align-to-paren nil)
;; (add-hook 'matlab-mode-hook #'abbrev-mode)

;; Use the Octave expression for iMenu that works better and finds all the
;; functions file.
;; (setq matlab-imenu-generic-expression
;;       (list (list nil octave-function-header-regexp 3)))

;;; Bazel
;; (add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
;; (add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

;;; SCons
(add-to-list 'auto-mode-alist '("\\SConscript*\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct*\\'" . python-mode))

;;; Line numbers

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

;;; Setup commenting.
(require 'evil-nerd-commenter)
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)

;;; Setup treesitter highlightning.
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'lyzell-programming)
