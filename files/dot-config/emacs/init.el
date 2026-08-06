;;; init.el --- Minimal Neovim-like Evil setup -*- lexical-binding: t; -*-

(require 'package)

;; These must be set before Evil (or evil-collection) is loaded.
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-undo-system 'undo-redo
      evil-want-C-u-scroll t
      evil-want-C-i-jump nil)

;; Keep package state alongside this XDG-based configuration.
(setq package-user-dir
      (expand-file-name
       "elpa/"
       (expand-file-name "emacs/"
                         (or (getenv "XDG_CONFIG_HOME") "~/.config/")))
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Keep recovery files out of project directories while retaining backups,
;; auto-saves, and lock files.  These are disposable XDG cache data.
(use-package emacs
  :ensure nil
  :config
  (let ((cache-directory
         (expand-file-name "emacs/"
                           (or (getenv "XDG_CACHE_HOME") "~/.cache/"))))
    (dolist (directory '("backups/" "auto-save/" "locks/"))
      (make-directory (expand-file-name directory cache-directory) t))
    (setq backup-directory-alist
          `(("." . ,(expand-file-name "backups/" cache-directory)))
          auto-save-file-name-transforms
          `((".*" ,(expand-file-name "auto-save/" cache-directory) t))
          lock-file-name-transforms
          `((".*" ,(expand-file-name "locks/" cache-directory) t))
          backup-by-copying t
          version-control t
          delete-old-versions t
          kept-new-versions 5
          kept-old-versions 2)))

;; Evil supplies Vim/Neovim editing states, motions, operators, registers,
;; text objects, and normal-mode commands across Emacs.
(use-package evil
  :demand t
  :config
  (evil-mode 1))

;; Use the familiar Vim comment operator: gc{motion}, with gcc for a line.
(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys))

;; Restore Vim's C-a/C-x number increment and decrement commands.
(use-package evil-numbers
  :after evil
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "C-a") #'evil-numbers/inc-at-pt
    (kbd "C-x") #'evil-numbers/dec-at-pt))

;; Use Space as a Doom-style leader while keeping only the two requested
;; project/file actions: Space-. and Space-,.
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "." #'find-file
    "," #'switch-to-buffer
    "r" #'my-recent-open
    "t" (lambda ()
          (interactive)
          (start-process "toggle-terminal" nil
                        (expand-file-name ".local/bin/toggle-terminal"
                                          (getenv "HOME"))))))

;; Add Evil bindings to built-in and third-party Emacs modes without adding a
;; separate integration package for each mode.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Minimal completion stack: Vertico UI, Orderless matching, persistent
;; minibuffer history, and Marginalia annotations.
;; Vertico provides the compact vertical minibuffer UI used for completion.
(use-package vertico
  :config
  (vertico-mode 1))

;; Orderless enables flexible component-wise matching; basic remains available
;; as a fallback and files retain partial-completion behavior.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

;; Save minibuffer history between sessions, stored in the XDG Emacs directory.
(use-package savehist
  :ensure nil
  :config
  (setq savehist-file
        (expand-file-name
         "history"
         (expand-file-name "emacs/"
                           (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))
  (savehist-mode 1))

;; Use Emacs' built-in recent-file list instead of adding another completion
;; package; Space-r opens it through the same minibuffer UI.
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file
        (expand-file-name
         "recentf"
         (expand-file-name "emacs/"
                           (or (getenv "XDG_CONFIG_HOME") "~/.config/")))
        recentf-max-saved-items 100)
  (recentf-mode 1)
  (defun my-recent-open ()
    "Open a recently visited file through the standard completion UI."
    (interactive)
    (if-let ((file (completing-read "Recent file: " recentf-list nil t)))
        (find-file file)
      (user-error "No recent file selected"))))

;; Show useful annotations such as file metadata and command descriptions in
;; the Vertico minibuffer.
(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

;; Fontaine manages persistent font presets using the installed Aporetic Nerd
;; Font for code and the Aporetic Sans family for variable-pitch text.
(use-package fontaine
  :config
  (setq fontaine-latest-state-file
        (expand-file-name
         "fontaine-latest-state.eld"
         (expand-file-name "emacs/"
                           (or (getenv "XDG_CONFIG_HOME") "~/.config/")))
        fontaine-presets
        '((small
           :default-family "AporeticSansMonoNerdFont"
           :default-height 90)
          (regular
           :default-family "AporeticSansMonoNerdFont"
           :default-height 140
           :fixed-pitch-family "AporeticSansMonoNerdFont"
           :variable-pitch-family "Aporetic Sans")
          (large
           :default-family "AporeticSansMonoNerdFont"
           :default-height 140)
          (t
           :default-family "AporeticSansMonoNerdFont"
           :default-height 140
           :fixed-pitch-family "AporeticSansMonoNerdFont"
           :variable-pitch-family "Aporetic Sans")))
  (fontaine-mode 1)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

;;; init.el ends here
