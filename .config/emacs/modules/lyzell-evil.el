;;; lyzell-evil.el -*- lexical-binding: t; -*-
;; TODO: Maybe use the undo-redo instead?

;;; Install dependencies.
(straight-use-package 'undo-tree)
(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-matchit)
(straight-use-package '(evil-numbers :host github
                                     :repo "cofi/evil-numbers"))
(straight-use-package 'evil-surround)
;;; Functions
(defun lyzell/open-init ()
  "Open the emacs init.el config file."
  (interactive)
  (find-file (expand-file-name "~/config/.config/emacs/init.el")))

(defun lyzell/open-nira ()
  "Open the nira.org file."
  (interactive)
  (find-file (expand-file-name "~/Documents/nira.org")))

;; Turn on undo-tree globally
;; (global-undo-tree-mode)

;;; Configure the evil package.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump t)
(setq evil-undo-system 'undo-redo)

(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-search-word-forward (around underscore-as-word1 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-search-word-backward (around underscore-as-word2 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-forward-word-begin (around underscore-as-word3 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-forward-word-end (around underscore-as-word4 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-backward-word-begin (around underscore-as-word5 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-backward-word-end (around underscore-as-word6 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-a-word (around underscore-as-word7 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))

(defadvice evil-inner-word (around underscore-as-word8 activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table ad-do-it)))


;; Setup evil mode.
(require 'evil)
(evil-mode 1)

(require 'evil-collection)
(evil-collection-init)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

;; Highlight the yanked region.
(customize-set-value 'evil-goggles-enable-change nil)
(customize-set-value 'evil-goggles-enable-commentary nil)
(customize-set-value 'evil-goggles-enable-delete nil)
(customize-set-value 'evil-goggles-enable-fill-and-move nil)
(customize-set-value 'evil-goggles-enable-indent nil)
(customize-set-value 'evil-goggles-enable-join nil)
(customize-set-value 'evil-goggles-enable-nerd-commenter nil)
(customize-set-value 'evil-goggles-enable-paste t)
(customize-set-value 'evil-goggles-enable-record-macro nil)
(customize-set-value 'evil-goggles-enable-replace-with-register nil)
(customize-set-value 'evil-goggles-enable-set-marker nil)
(customize-set-value 'evil-goggles-enable-shift nil)
(customize-set-value 'evil-goggles-enable-surround nil)
(customize-set-value 'evil-goggles-enable-yank t)
(customize-set-value 'evil-goggles-duration 0.3)
(customize-set-value 'evil-goggles-pulse t)
(evil-goggles-mode)


;; Add underscore to the word list.
(require 'prog-mode)
(modify-syntax-entry ?_ "w" prog-mode-syntax-table)
(require 'text-mode)
(modify-syntax-entry ?_ "w" text-mode-syntax-table)
(require 'cc-mode)
(modify-syntax-entry ?_ "w" c-mode-syntax-table)
(modify-syntax-entry ?_ "w" c++-mode-syntax-table)
(require 'octave)
(modify-syntax-entry ?_ "w" octave-mode-syntax-table)
(require 'rst)
(modify-syntax-entry ?_ "w" rst-mode-syntax-table)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;;; Keybindings
(global-set-key (kbd "C-S-u") 'universal-argument)
;; (evil-define-key 'insert 'global (kbd "C-h") 'evil-delete-backward-char-and-join)
(evil-define-key 'normal 'global (kbd "gc")  'evilnc-comment-operator)
(evil-define-key 'normal 'global (kbd "s-x") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "-")   'dired-jump)
(evil-define-key 'insert 'global (kbd "C-g") 'evil-normal-state)
(evil-define-key 'visual 'global (kbd "C-g") 'evil-normal-state)

(defun lyzell-open-terminal-here ()
  "Open a terminal in the default directory"
  (interactive)
  (vterm (concat "term " default-directory)))

;;; Leader keybindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'normal (kbd "C-SPC"))
(evil-set-leader 'insert (kbd "C-SPC"))
(evil-set-leader 'visual (kbd "SPC"))
(evil-set-leader 'visual (kbd "C-SPC"))
(evil-define-key 'normal 'global (kbd "<leader>q")     'save-buffers-kill-terminal)
(evil-define-key 'normal 'global (kbd "<leader>.")     'find-file)
(evil-define-key 'normal 'global (kbd "<leader>,")     'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>SPC")   'evil-switch-to-windows-last-buffer)
(evil-define-key 'normal 'global (kbd "<leader>c")     'evil-window-delete)
(evil-define-key 'normal 'global (kbd "<leader>d")     'evil-delete-buffer)
(evil-define-key 'normal 'global (kbd "<leader>C-SPC") 'evil-switch-to-windows-last-buffer)
(evil-define-key 'normal 'global (kbd "<leader>tf")    'consult-theme)
(evil-define-key 'normal 'global (kbd "<leader>tt")    'modus-themes-toggle)
(evil-define-key 'normal 'global (kbd "<leader>tc")    'hl-line-mode)
(evil-define-key 'normal 'global (kbd "<leader>tr")    'lyzell/toggle-relative-line-numbers)
(evil-define-key 'normal 'global (kbd "<leader>tw")    'toggle-truncate-lines)
(evil-define-key 'normal 'global (kbd "<leader>ae")    'eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>ap")    'proced)
(evil-define-key 'normal 'global (kbd "<leader>ac")    'lyzell/open-init)
(evil-define-key 'normal 'global (kbd "<leader>an")    'lyzell/open-nira)
(evil-define-key 'normal 'global (kbd "<leader>ab")    'ibuffer)
(evil-define-key 'normal 'global (kbd "<leader>af")    'find-file)
(evil-define-key 'normal 'global (kbd "<leader>as")    'eshell)
(evil-define-key 'normal 'global (kbd "<leader>at")    'lyzell-open-terminal-here)
(evil-define-key 'normal 'global (kbd "<leader>ad")    'dired)
(evil-define-key 'normal 'global (kbd "<leader>aw")    'eww)
(evil-define-key 'normal 'global (kbd "<leader>p.")    'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>p,")    'project-switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>pc")    'project-compile)
(evil-define-key 'normal 'global (kbd "<leader>pr")    'recompile)
(evil-define-key 'normal 'global (kbd "<leader>pp")    'project-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pad")   'project-dired)
(evil-define-key 'normal 'global (kbd "<leader>pas")   'project-eshell)
(evil-define-key 'normal 'global (kbd "<leader>gb")    'magit-blame)
(evil-define-key 'normal 'global (kbd "<leader>gc")    'magit-diff-buffer-file)
(evil-define-key 'normal 'global (kbd "<leader>gs")    'magit)
(evil-define-key 'normal 'global (kbd "<leader>gl")    'vc-print-root-log)
(evil-define-key 'normal 'global (kbd "<leader>gw")    'lyzell/vc-grep)
(evil-define-key 'normal 'global (kbd "<leader>w")     'evil-write)
(evil-define-key 'normal 'global (kbd "C-c i")         'evil-numbers/inc-at-pt)
(evil-define-key 'normal 'global (kbd "C-c d")         'evil-numbers/dec-at-pt)
(evil-define-key 'visual 'global (kbd "<leader>aa")     'align-regexp)
(evil-define-key 'visual 'global (kbd "<leader>ae")    'eval-region)

;; Remap the, for the Dvorak layout, horrible `x`-key.  (define-key
;; evil-normal-state-map (kbd "RET") ctl-x-map)

;; (evil-define-key 'normal 'global (kbd "M-m") 'execute-extended-command)
;; (evil-define-key 'insert 'global (kbd "M-m") 'execute-extended-command)
;; (evil-define-key 'visual 'global (kbd "M-m") 'execute-extended-command)

(provide 'lyzell-evil)
