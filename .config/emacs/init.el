;;; init.el --- Init File -*- lexical-binding: t -*-

;;; Improvement suggestions
;; TODO: COMPLETION - How to make completion better, work more like nvim?
;; TODO: MAGIT      - Better git blame?
;; TODO: PROJECT    - Compile commands selection using vertico?
;; TODO: HARPOON    - How to do with Harpoon? The available package is not working
;;                    as expected by separating the files per project.
;; TODO: EDIFF      - How to setup ediff to work more in the way I am used to and like?
;; TODO: VDIFF      - This seems to be the tool that I want to use, make sure it works like I expect.
;; TODO: OUTLINE    - Make the cursor move to the place where the visual cursor is positioned.
;; TODO: KEYBINDS   - How to do with the AltGr key?
;; TODO: ESHELL     - How to make git not use a pager?

;;; Start-up time
;; Measure the start-up time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;;; Initialize the package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Universal argument
(global-set-key (kbd "M-g") 'universal-argument)

;;; Load my modules
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(require 'lyzell-editing)
(require 'lyzell-evil)
(require 'lyzell-selection)
(require 'lyzell-proced)
(require 'lyzell-programming)
(require 'lyzell-vc)
(require 'lyzell-terminal)
(require 'lyzell-org)

(when 'native-comp-compiler-options
  (setq native-comp-speed 3
        native-comp-compiler-options '("-O3" "-mtune=native")))

;;; Settings
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Focus the help window.
(customize-set-variable 'help-window-select t)

;; Delete files to trash
(customize-set-variable 'delete-by-moving-to-trash t)

;; Take new window space from all other windows (not just current)
(setq-default window-combination-resize t)

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

;; This needs to be explored.
(straight-use-package 'rg)
(require 'rg)

;;; Perspective
;; This needs to be explored.
(straight-use-package 'perspective)
(setq persp-suppress-no-prefix-key-warning t)
(persp-mode)
(evil-define-key 'normal 'global (kbd "<leader>ps")    'persp-switch)
(evil-define-key 'normal 'global (kbd "<leader>pl")    'persp-switch-last)
(evil-define-key 'normal 'global (kbd "<leader>pk")    'persp-kill)

;; (defun lyzell/git-merge-base ()
  ;; "Find the merge-base."
  ;; (interactive)
  ;; (start-process "

;;; Corfu
;; Maybe to be used as an auto-completion.
(straight-use-package 'corfu)
(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t
(global-corfu-mode)
;; (setq-local completion-at-point-functions
;;             (mapcar #'cape-company-to-capf
;;                     (list #'company-files #'company-ispell #'company-dabbrev)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

;;; TODO: Organize this
(straight-use-package 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(dolist (mode '(elisp-mode))
  (add-hook mode (lambda () (add-to-list 'completion-at-point-functions #'cape-symbol))))

(straight-use-package 'company)
(require 'company)

(straight-use-package 'lsp-mode)

(straight-use-package 'dabbrev)

;; How to to this, is the googles implementation better than the evil-goggles?
;; (straight-use-package 'goggles)
;; (setq-default goggles-pulse t)

(setq help-window-select nil)

;;; Theme
(straight-use-package '(modus-themes
                        :type git
                        :repo "https://github.com/protesilaos/modus-themes"))

(setq modus-themes-italic-constructs nil)
(setq modus-themes-mode-line '(padded))
;; (setq modus-themes-syntax '(yellow-comments))
(setq modus-themes-syntax nil)
(load-theme 'modus-vivendi t nil)

;;; Fonts
;; Fonts that I like:
;; - `JetBrainsMono Nerd Font'
;; - `Sauce Code Pro Nerd Font'
;; - `Iosevka Comfy Wide Fixed'
(set-face-attribute 'default nil
                    :font "Iosevka Comfy Wide Fixed:antialias=subpixel"
                    :weight 'regular
                    :height 110)

;;; Basic settings
(setq frame-title-format '("%b"))	; set the title of the emacs window
(setq ring-bell-function 'ignore)	; no ring bell function
(setq use-short-answers t)		; shorten the yes or no questions

;; (defun e-run-command ()
;;   "Run external system programs. Dmenu/Rofi-like.  Tab/C-M-i to completion
;; n-[b/p] for walk backward/forward early commands history."
;;   (interactive)
;;   (require 'subr-x)
;;   (start-process "RUN" "RUN" (string-trim-right (read-shell-command "RUN: "))))

;;; Scrolling
;; Need to patch pgtkterm.c:6715
;;   dpyinfo->scroll.x_per_char = 1.0;
;;   dpyinfo->scroll.y_per_line = 1.0;
(setq scroll-margin 0)	                            ; add space at the top and the bottom
(setq scroll-conservatively 101)	            ; do not center the cursor when scrolling past the screen
(setq scroll-preserve-screen-position nil)          ; do not move the cursor when scrolling, except at the top or the bottom of the screen
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; scroll three lines at a time, same as in the terminal
(setq mouse-wheel-progressive-speed nil)	    ; do not accelerate the scolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll the window under the mouse

;;; Outline minor mode
;; Make ESC quit prompts
;; (let ((map outline-minor-mode-map))
;; ;; NOTE 2021-07-25: Those two are already defined (emacs28).
;; (evil-define-key 'normal map (kbd "TAB") 'outline-cycle))
;; (define-key map (kbd "TAB") #'outline-cycle)
;; (define-key map (kbd "<backtab>") #'outline-cycle-buffer))
;;   (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
;;   (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
;;   (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
;;   (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
;;   (define-key map (kbd "C-c C-a") #'outline-show-all)
;;   (define-key map (kbd "C-c C-o") #'outline-hide-other)
;;   (define-key map (kbd "C-c C-u") #'outline-up-heading)))

;;;; Personal functions
(defun lyzell/outline-cycle ()
  "Toggle the outline cycle.
TODO: Make it move the cursor to the where the visual cursor is."
  (interactive)
  (if (bound-and-true-p outline-minor-mode)
      (outline-cycle)
    (org-cycle)))

(defun lyzell/outline-cycle-buffer ()
  "Cycle the visability of the buffer.
TODO: Make it move the cursor to the where the visual cursor is."
  (interactive)
  (if (bound-and-true-p outline-minor-mode)
      (outline-cycle-buffer)
    (org-shifttab)))

(defun lyzell/outline-heading ()
  "Imenu like functionality."
  (interactive)
  (if (bound-and-true-p outline-minor-mode)
      (consult-outline)
    (if (bound-and-true-p org-mode)
        (consult-org-heading)
      (consult-imenu))))

;;;; Prot functions
(defcustom prot-outline-headings-per-mode
  '((emacs-lisp-mode . ";\\{3,\\}+ [^\n]"))
  "Alist of major modes with `outline-regexp' values."
  :type '(alist :key-type symbol :value-type string)
  :group 'prot-outline)

(defcustom prot-outline-major-modes-blocklist
  '(org-mode outline-mode markdown-mode)
  "Major modes where Outline-minor-mode should not be enabled."
  :type '(repeat symbol)
  :group 'prot-outline)

(defun prot-outline-minor-mode-safe ()
  "Test to set variable `outline-minor-mode' to non-nil."
  (interactive)
  (let* ((blocklist prot-outline-major-modes-blocklist)
         (mode major-mode)
         (headings (alist-get mode prot-outline-headings-per-mode)))
    (when (derived-mode-p (car (member mode blocklist)))
      (error "Don't use `prot-outline-minor-mode' with `%s'" mode))
    (if (null outline-minor-mode)
        (progn
          (when (derived-mode-p mode)
            (setq-local outline-regexp headings))
          (outline-minor-mode 1)
          (message "Enabled `outline-minor-mode'"))
      (outline-minor-mode -1)
      (message "Disabled `outline-minor-mode'"))))

;;;; Configuration
(require 'outline)
(setq outline-minor-mode-highlight 'override)
(setq outline-minor-mode-cycle t)

(evil-define-key 'normal 'global (kbd "M-C-x") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "TAB") 'lyzell/outline-cycle)
(evil-define-key 'normal 'global (kbd "<backtab>") 'lyzell/outline-cycle-buffer)
(evil-define-key 'normal 'global (kbd "<leader>'") 'lyzell/outline-heading)
(evil-define-key 'normal 'global (kbd "<leader>;") 'lyzell/outline-heading)
(evil-define-key 'normal 'global (kbd "<leader>ao") 'prot-outline-minor-mode-safe)
(evil-define-key 'normal 'global (kbd "<leader>a-") 'text-scale-decrease)
(evil-define-key 'normal 'global (kbd "<leader>a_") 'text-scale-increase)
(evil-define-key 'normal 'global (kbd "<leader>a+") 'text-scale-increase)
(let ((map outline-minor-mode-map))
  (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
  (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
  (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
  (define-key map (kbd "C-c C-a") #'outline-show-all)
  (define-key map (kbd "C-c C-o") #'outline-hide-other)
  (define-key map (kbd "C-c C-u") #'outline-up-heading))

;;; TODO: Organize this
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(straight-use-package '(avy :host github
                            :repo "abo-abo/avy"))
(evil-define-key 'normal 'global (kbd "<leader>j")    'avy-goto-line)
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

;; TODO: This should be removed and maybe replaced by embark.
(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; (use-package magit
;;  :custom magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
;;  )

(require 'subr-x)

(defun lyzell/git-get-current-branch ()
  "Get the current revision."
  (car (split-string
    (shell-command-to-string
     "git rev-parse --abbrev-ref HEAD"))))

(defun lyzell/git-get-merge-base ()
  "Get the merge base of the current revision."
  (interactive)
  (car (split-string
        (shell-command-to-string
          "git merge-base HEAD dev/master"))))

(defun lyzell/print-merge-base ()
  "Get the merge base of the current revision."
  (interactive)
  (message (lyzell/git-get-merge-base)))

(defun lyzell/git-diff-merge-base ()
  "Get the diff against the merge base."
  (interactive)
  (magit-diff-range (lyzell/git-get-merge-base)))

;; (lyzell/git-get-current-branch)
;; (lyzell/git-get-merge-base)

;;; PDF-tools
(straight-use-package 'pdf-tools)
(setq pdf-tools-enabled-modes         ; simplified from the defaults
      '(pdf-history-minor-mode
        pdf-isearch-minor-mode
        pdf-links-minor-mode
        pdf-outline-minor-mode
        pdf-misc-size-indication-minor-mode
        pdf-occur-global-minor-mode))
(setq pdf-view-display-size 'fit-height)
(setq pdf-view-continuous t)
(setq pdf-view-use-dedicated-register nil)
(setq pdf-view-max-image-width 1080)
(setq pdf-outline-imenu-use-flat-menus t)

(pdf-loader-install)

;; Those functions and hooks are adapted from the manual of my
;; modus-themes.  The idea is to (i) add a backdrop that is distinct
;; from the background of the PDF's page and (ii) make pdf-tools adapt
;; to theme switching via, e.g., `modus-themes-toggle'.
(defun prot/pdf-tools-backdrop ()
  (face-remap-add-relative
   'default
   `(:background ,(modus-themes-color 'bg-alt))))

(defun prot/pdf-tools-midnight-mode-toggle ()
  (when (derived-mode-p 'pdf-view-mode)
    (if (eq (car custom-enabled-themes) 'modus-vivendi)
        (pdf-view-midnight-minor-mode 1)
      (pdf-view-midnight-minor-mode -1))
    (prot/pdf-tools-backdrop)))

(add-hook 'pdf-tools-enabled-hook #'prot/pdf-tools-midnight-mode-toggle)
(add-hook 'modus-themes-after-load-theme-hook #'prot/pdf-tools-midnight-mode-toggle)

;;; Get rid of some annoying files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)
(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

(straight-use-package 'helpful)
(require 'helpful)
(evil-define-key 'normal 'global (kbd "C-h f") 'helpful-callable)
(evil-define-key 'normal 'global (kbd "C-h v") 'helpful-variable)

(straight-use-package 'vterm)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Project

(defun lyzell-project-vterm ()
  "Start vterm in the current project's root directory.
If a buffer already exists for running vterm in the project's root,
switch to it.  Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm t))))

(evil-define-key 'normal 'global (kbd "<leader>pat")   'lyzell-project-vterm)

;;; Dired
(require 'dired)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches
      "-AGFhlv --group-directories-first --time-style=long-iso")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
(setq dired-make-directory-clickable t)
(setq dired-free-space nil)
(setq dired-mouse-drag-files t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

;; Allow the use of the alternate file to keep the number of dired buffers to a
;; minimum and not clutter the buffer list.
(put 'dired-find-alternate-file 'disabled nil)

(defun lyzell/find-parent-directory ()
  "Find the parent directory."
  (interactive)
  (find-alternate-file ".."))

(defun lyzell-bury-restore-window ()
  "Kill the window."
  (interactive)
  (quit-restore-window nil 'bury))

(defun lyzell-kill-restore-window ()
  "Kill the window."
  (interactive)
  (quit-restore-window nil 'kill))

(defun lyzell-save-kill-restore-window ()
  "Save the buffer and kill the window."
  (interactive)
  (save-buffer)
  (quit-restore-window nil 'kill))

;; (evil-define-key 'normal 'global (kbd "C-c k") 'lyzell-kill-restore-window)
(evil-define-key 'normal 'global (kbd "<leader> q") 'lyzell-kill-restore-window)

(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-alternate-file))
(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "-")   'lyzell/find-parent-directory))
(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'helpful
  '(evil-define-key 'normal helpful-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'help
  '(evil-define-key 'normal help-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'ibuffer
  '(evil-define-key 'normal ibuffer-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'magit
  '(evil-define-key 'normal magit-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'proced
  '(evil-define-key 'normal proced-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'info
  '(evil-define-key 'normal Info-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'diff
  '(evil-define-key 'normal diff-mode-map (kbd "gq")
     'lyzell-bury-restore-window))
(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "SPC")
     'evil-send-leader))
(eval-after-load 'magit
  '(evil-define-key 'normal magit-mode-map (kbd "SPC")
     'evil-send-leader))
(eval-after-load 'info
  '(evil-define-key 'normal Info-mode-map (kbd "SPC")
     'evil-send-leader))
(eval-after-load 'xref
  '(evil-define-key 'normal Info-mode-map (kbd "SPC")
     'evil-send-leader))
(eval-after-load 'eww
  '(evil-define-key 'normal eww-mode-map (kbd "SPC")
     'evil-send-leader))
(eval-after-load 'proced
  '(evil-define-key 'normal proced-mode-map (kbd "SPC")
     'evil-send-leader))

(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'helpful
  '(evil-define-key 'normal helpful-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'help
  '(evil-define-key 'normal help-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'ibuffer
  '(evil-define-key 'normal ibuffer-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'magit
  '(evil-define-key 'normal magit-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'proced
  '(evil-define-key 'normal proced-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'info
  '(evil-define-key 'normal Info-mode-map (kbd "q")
     'lyzell-kill-restore-window))
(eval-after-load 'diff
  '(evil-define-key 'normal diff-mode-map (kbd "q")
     'lyzell-kill-restore-window))

;; Do not open the async shell command buffer.
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(require 'dired-x)
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "zathura")
        ("\\.7z\\'" "xdg-open")
        ("\\.xlsx\\'" "libreoffice")))

;;; Magit
(require 'magit)
(require 'evil-collection)

(straight-use-package 'rust-mode)
(straight-use-package 'rustic)

(defun lyzell/forward-section ()
  "Change the behaviour of forward section depending on the major mode."
  (interactive)
  (if (equal major-mode 'magit-status-mode)
      (magit-section-forward-sibling)
    (evil-forward-section-begin)))

(defun lyzell/backward-section ()
  "Change the behaviour of backward section depending on the major mode."
  (interactive)
  (if (equal major-mode 'magit-status-mode)
      (magit-section-backward-sibling)
    (evil-backward-section-begin)))

;; Do not add the backward/forward section movements to the evil-repeat command.
(require 'evil)
(evil-declare-abort-repeat 'lyzell/backward-section)
(evil-declare-abort-repeat 'lyzell/forward-section)

;; Is this the way to do it? It works, but it is not beautiful.
(evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
  "[[" 'lyzell/backward-section
  "]]" 'lyzell/forward-section)

;;; Vdiff
(straight-use-package 'vdiff)

;;; Lin

(straight-use-package 'lin)

(require 'lin)

(setq lin-face 'lin-blue) ; check doc string for alternative styles

;; You can use this to live update the face:
;;
;; (customize-set-variable 'lin-face 'lin-green)

(setq lin-mode-hooks
      '(bongo-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        grep-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        magit-log-mode-hook
        mu4e-headers-mode
        notmuch-search-mode-hook
        notmuch-tree-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        proced-mode-hook
        tabulated-list-mode-hook))

(lin-global-mode 1)

;;; Pulsar
;; This is mostly due to that pulsar does not respect evil or maybe more likely
;; the other way around.
(straight-use-package 'pulsar)
(require 'pulsar)

(setq pulsar-pulse-functions
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        org-next-visible-heading
        org-previous-visible-heading
        org-forward-heading-same-level
        org-backward-heading-same-level
        outline-backward-same-level
        outline-forward-same-level
        outline-next-visible-heading
        outline-previous-visible-heading
        outline-up-heading))

(pulsar-global-mode 1)

(defun lyzell-scroll-down ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-scroll-down 0)
  (pulsar-pulse-line))

(defun lyzell-scroll-up ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-scroll-up 0)
  (pulsar-pulse-line))

(defun lyzell-scroll-line-bottom ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-scroll-line-to-bottom (line-number-at-pos))
  (pulsar-pulse-line))

(defun lyzell-window-top ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-window-top)
  (pulsar-pulse-line))

(defun lyzell-window-bottom ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-window-bottom)
  (pulsar-pulse-line))

(defun lyzell-window-middle ()
  "Simple wrapper to work with pulse."
  (interactive)
  (evil-window-middle)
  (pulsar-pulse-line))

(evil-define-key 'normal 'global (kbd "C-d") 'lyzell-scroll-down)
(evil-define-key 'normal 'global (kbd "C-u") 'lyzell-scroll-up)
(evil-define-key 'normal 'global (kbd "zb")  'lyzell-scroll-line-bottom)
(evil-define-key 'normal 'global (kbd "zz")  'pulsar-recenter-middle)
(evil-define-key 'normal 'global (kbd "zt")  'pulsar-recenter-top)
(evil-define-key 'normal 'global (kbd "H")   'lyzell-window-top)
(evil-define-key 'normal 'global (kbd "L")   'lyzell-window-bottom)
(evil-define-key 'normal 'global (kbd "M")   'lyzell-window-middle)

;; (straight-use-package 'hl-todo)
;; (global-hl-todo-mode)

;; Improve this to first kill to first character (matching the indentation).
(defun lyzell-kill-beginning-line ()
  (interactive)
  (kill-line 0))

(evil-define-key 'insert 'global (kbd "C-u") 'lyzell-kill-beginning-line)

(require 'hi-lock)

;; Improve this to be able to mark several words.
(defun lyzell-toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(evil-define-key 'normal 'global (kbd "g.") 'lyzell-toggle-mark-word-at-point)

;; (straight-use-package 'projectile)
;; (require 'projectile)

;;; Harpoon
;; TODO: maybe implement this myself
;; TODO
(straight-use-package 'harpoon)
(require 'harpoon)
(require 'project)
;; (custom-set-variables '(harpoon-separate-by-branch t))
(customize-set-variable 'harpoon-project-package 'project)
(evil-define-key 'normal 'global (kbd "<leader>ua")  'harpoon-add-file)
(evil-define-key 'normal 'global (kbd "<leader>um")  'harpoon-toggle-file)
(evil-define-key 'normal 'global (kbd "<leader>uc")  'harpoon-clear)
(evil-define-key 'normal 'global (kbd "<leader>uh")  'harpoon-go-to-1)
(evil-define-key 'normal 'global (kbd "<leader>ut")  'harpoon-go-to-2)
(evil-define-key 'normal 'global (kbd "<leader>un")  'harpoon-go-to-3)
(evil-define-key 'normal 'global (kbd "<leader>us")  'harpoon-go-to-4)
(eval-after-load 'harpoon
  '(evil-define-key 'normal harpoon-mode-map (kbd "q") 'lyzell-save-kill-restore-window))

;; Set the garbage collection threshold for faster performance.
;; (setq gc-cons-threshold 800000)
(setq gc-cons-threshold 2 * 1000 * 1000)

;; tarsius/hl-todo

;; (defun lyzell-find-file ()
;;   (interactive)
;;   (
;;   (message (project-current))

;;; Customize variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" default))
 '(enable-recursive-minibuffers t)
 '(fringe-mode 10 nil (fringe))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/Documents/nira.org"))
 '(package-selected-packages
   '(magit evil-nerd-commenter perspective general which-key tree-sitter-lang vterm vertico use-package tree-sitter-langs orderless marginalia good-scroll evil-collection doom-themes consult company))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
