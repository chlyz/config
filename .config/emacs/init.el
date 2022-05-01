;;; init.el --- Init File -*- lexical-binding: t -*-
;; TODO: Checkout hs-minor-mode
;; TODO: How to setup abbreviations?
;; TODO: How to setup ediff to work more in the way I am used to and like?
;; TODO: How to do Harpoon? I really like nvim Harpoon. This must be a plugin of its own.
;; TODO: info-mode - remove the space key mapping to use as a leader key instead and do I really want that?
;; TODO: dired needs to be fixed, maybe close the dired buffer when selecting a file or going forwards and backwards? keep the alternate file
;; TODO: magit blame can be cycled to the format i like with `c`. How to make this the default style?
;; TODO: magit windows do not get closed when leaving. How to fix that so that the buffer list is not so cluttered?
;; TODO: ibuffer windows do not get closed when leaving. How to fix that so that the buffer list is not so cluttered?
;; TODO: make a project vterm, similar to the eshell version.
;; TODO: how to make use of the different terminal alternatives?
;; TODO: how to make completion better, work more like nvim?
;; TODO: how to improve the cursor line to only be enabled in the buffers that I want

;; Measure the start-up time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Initialize the straight package manager.
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

;; Load the modules.
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(require 'lyzell-editing)
(require 'lyzell-evil)
(require 'lyzell-selection)
(require 'lyzell-proced)
(require 'lyzell-programming)
(require 'lyzell-vc)
(require 'lyzell-terminal)

;; This needs to be explored.
(straight-use-package 'rg)
(require 'rg)

;; This needs to be explored.
(straight-use-package 'perspective)
(setq persp-suppress-no-prefix-key-warning t)
(persp-mode)

;; (defun lyzell/git-merge-base ()
  ;; "Find the merge-base."
  ;; (interactive)
  ;; (start-process "

;; Maybe to be used as an auto-completion.
;; (straight-use-package 'corfu)
;; (setq corfu-auto t
;;       corfu-quit-no-match 'separator) ;; or t
;; (corfu-global-mode)
;; (require 'corfu)
;; (setq-local completion-at-point-functions
;;             (mapcar #'cape-company-to-capf
;;                     (list #'company-files #'company-ispell #'company-dabbrev)))

(straight-use-package 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(dolist (mode '(elisp-mode))
  (add-hook mode (lambda () (add-to-list 'completion-at-point-functions #'cape-symbol))))

(straight-use-package 'company)
(require 'company)

(straight-use-package 'lsp-mode)

(straight-use-package 'dabbrev)

(setq help-window-select nil)

;;; Theme
(setq modus-themes-italic-constructs nil)
(load-theme 'modus-operandi t nil)

;;; Fonts
(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font:antialias=subpixel"
                    :height 100)

;;; Basic settings
(setq frame-title-format '("%b"))	; set the title of the emacs window
(setq ring-bell-function 'ignore)	; no ring bell function
(setq use-short-answers t)		; shorten the yes or no questions

;;; Scrolling
;; Need to patch pgtkterm.c:6715
;;   dpyinfo->scroll.x_per_char = 1.0;
;;   dpyinfo->scroll.y_per_line = 1.0;
(setq scroll-margin 3)	                            ; add space at the top and the bottom
(setq scroll-conservatively 101)	            ; do not center the cursor when scrolling past the screen
(setq scroll-preserve-screen-position nil)          ; do not move the cursor when scrolling, except at the top or the bottom of the screen
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; scroll three lines at a time, same as in the terminal
(setq mouse-wheel-progressive-speed nil)	    ; do not accelerate the scolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll the window under the mouse

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; TODO: This should be removed and maybe replaced by embark.
(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.3)

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
  (car (split-string
        (shell-command-to-string
          "git merge-base HEAD dev/master"))))

(defun lyzell/git-diff-merge-base ()
  "Get the diff against the merge base."
  (interactive)
  (magit-diff-range (lyzell/git-get-merge-base)))

;; (lyzell/git-get-current-branch)
;; (lyzell/git-get-merge-base)

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

;;;; Org-mode
(customize-set-value 'org-log-into-drawer t)
(customize-set-value 'org-clock-into-drawer "CLOCKING")
(customize-set-value 'org-tags-column -78)
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "MEET(m)" "WAIT(w)" "IDEA(i)" "PROJECT(p)" "|" "DONE(d)" "DROP(c)")))
(require 'org)
(evil-define-key 'normal 'org-mode-map (kbd "<leader>oh")  'consult-org-heading)
(evil-define-key 'normal 'org-mode-map (kbd "<leader>oa")  'org-agenda)

;; Set the garbage collection threshold for faster performance.
(setq gc-cons-threshold (* 2 1000 1000))

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
