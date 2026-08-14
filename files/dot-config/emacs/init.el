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

;; Keep file-visiting buffers in sync when another program edits their files.
;; Emacs uses file-notification events where they are available, and otherwise
;; checks the file periodically without interrupting the user.  Also check as
;; soon as Emacs regains focus after an external editor, such as Codex, writes
;; to a visited file.
(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode 1)
  (defun my-auto-revert-on-focus ()
    "Refresh externally changed buffers when Emacs regains focus."
    (let ((auto-revert-stop-on-user-input nil))
      (auto-revert-buffers)))
  (add-hook 'focus-in-hook #'my-auto-revert-on-focus))

;; Evil supplies Vim/Neovim editing states, motions, operators, registers,
;; text objects, and normal-mode commands across Emacs.
(use-package evil
  :demand t
  :config
  (evil-mode 1)
  (defun my-save-buffer-and-normal-state ()
    "Save the current buffer and return to Evil normal state."
    (interactive)
    (save-buffer)
    (evil-normal-state))
  (evil-define-key 'normal 'global (kbd "C-s") #'save-buffer)
  (evil-define-key 'insert 'global (kbd "C-s")
    #'my-save-buffer-and-normal-state))

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
                                          (getenv "HOME")))))
  (evil-leader/set-key-for-mode 'org-mode
    "oi" #'org-clock-in
    "oo" #'org-clock-out
    "or" #'org-clock-report
    "ot" #'org-todo
    "os" #'org-schedule))

;; Add Evil bindings to built-in and third-party Emacs modes without adding a
;; separate integration package for each mode.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil uses Emacs' paragraph variables for `ip' and `ap'.  Org changes those
;; variables for structural navigation, so advise only these text objects to
;; use the same blank-line paragraph boundaries as a plain-text buffer.
(defun my-org-plain-text-paragraph (original count &optional beg end type)
  "Use plain-text paragraph bounds for Evil text objects in Org buffers."
  (if (derived-mode-p 'org-mode)
      (let ((paragraph-start "\f\\|[ \t]*$")
            (paragraph-separate "[ \t\f]*$"))
        (funcall original count beg end type))
    (funcall original count beg end type)))

(dolist (function '(evil-inner-paragraph evil-a-paragraph))
  (unless (advice-member-p #'my-org-plain-text-paragraph function)
    (advice-add function :around #'my-org-plain-text-paragraph)))

;; Keep Org entries readable on open while folding drawers such as LOGBOOK.
(use-package org
  :ensure nil
  :custom
  (org-startup-folded 'nofold)
  (org-cycle-hide-drawer-startup t))

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

;; A Perspective is a named, persistent workspace with its own buffer list and
;; window layout.  This makes a project feel like a tmux session: files,
;; compilation buffers, and shells from other projects stay out of the way.
(use-package perspective
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x C-b" . persp-list-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-nil-name "main")
  (persp-modestring-short t)
  (persp-sort 'created)
  (persp-state-default-file
   (expand-file-name
    "perspectives.el"
    (expand-file-name "emacs/"
                      (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))
  :config
  (persp-mode 1)
  (setq switch-to-prev-buffer-skip
        (lambda (_window buffer _bury-or-kill)
          (not (persp-is-current-buffer buffer))))
  (when (file-exists-p persp-state-default-file)
    (persp-state-load persp-state-default-file))
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; Project.el locates the project root while Perspective preserves its working
;; context.  Space-p opens a known project (or accepts a new directory), then
;; switches to its workspace and prompts for a project file.
(use-package project
  :ensure nil
  :config
  (defun my-project-open-perspective ()
    "Open the current project in its named Perspective workspace."
    (let* ((root (project-root (project-current t)))
           (name (concat "project:"
                         (abbreviate-file-name (directory-file-name root)))))
      (let ((default-directory root))
        (persp-switch name)
        (project-find-file))))

  (setq project-switch-commands
        '((my-project-open-perspective "Open project workspace")))

  (defun my-project-switch ()
    "Select a project and open its Perspective workspace."
    (interactive)
    (project-switch-project (project-prompt-project-dir)))

  (evil-leader/set-key
    "p" #'my-project-switch
    "," #'persp-switch-to-buffer*))

;; Markdown receives the same restrained, semantic treatment as Org: structural
;; markup and code are distinct without adding custom faces or decoration.
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t))

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
