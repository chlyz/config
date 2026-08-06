;;; early-init.el --- Startup UI defaults -*- lexical-binding: t; -*-

;; Keep Emacs' caches and generated state in the XDG configuration directory,
;; rather than recreating ~/.emacs.d.
(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name "emacs/"
                         (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))

;; Let init.el control package initialization so packages are not initialized
;; before Evil's configuration variables are set.
(setq package-enable-at-startup nil)

;; Keep startup allocation and filesystem checks from triggering repeated
;; pauses.  Restore practical values once the initial frame is ready.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(defvar my--file-name-handler-alist file-name-handler-alist)
(defvar my--vc-handled-backends vc-handled-backends)
(setq file-name-handler-alist nil
      vc-handled-backends nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my--file-name-handler-alist
                  vc-handled-backends my--vc-handled-backends)))

;; Compile asynchronously without interrupting startup with compiler warning
;; buffers.  The diagnostic buffers remain available for manual inspection.
(setq native-comp-async-report-warnings-errors 'silent)
(dolist (buffer-regexp '("\\*Warnings\\*" "\\*Compile-Log\\*"))
  (add-to-list 'display-buffer-alist
               `(,buffer-regexp
                 (display-buffer-no-window)
                 (allow-no-window . t))))

(setq initial-buffer-choice "*scratch*"
      initial-scratch-message nil
      inhibit-startup-screen t)

;; Load the saved built-in Modus theme before the first frame is shown.  This
;; avoids the light-background flash when using a dark theme.
(setq custom-file
      (expand-file-name
       "custom.el"
       (expand-file-name "emacs/"
                         (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))

(defgroup my-modus nil
  "Personal Modus theme settings."
  :group 'faces)

(defcustom my-modus-theme 'modus-operandi-tinted
  "The built-in Modus theme loaded at startup."
  :type '(choice (const modus-operandi-tinted)
                 (const modus-vivendi-tinted)
                 (const modus-operandi)
                 (const modus-vivendi)
                 (const modus-operandi-deuteranopia)
                 (const modus-operandi-tritanopia)
                 (const modus-vivendi-deuteranopia)
                 (const modus-vivendi-tritanopia))
  :group 'my-modus)

(when (file-exists-p custom-file)
  (load custom-file nil nil))

;; Use the shared desktop theme selection when one has been set externally.
(let ((theme-state
       (expand-file-name
        "modus-theme/current"
        (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))
  (when (file-exists-p theme-state)
    (setq my-modus-theme
          (intern (string-trim (with-temp-buffer
                                 (insert-file-contents theme-state)
                                 (buffer-string)))))))

(load-theme my-modus-theme t)

;; Follow Prot's approach: use a neutral dark frame while the first frame is
;; created, then re-enable the selected theme once the frame exists.
(defun my-modus-re-enable-frame-theme (_frame)
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(when (memq my-modus-theme
            '(modus-vivendi modus-vivendi-tinted
              modus-vivendi-deuteranopia modus-vivendi-tritanopia))
  (setq mode-line-format nil)
  (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
  (set-face-attribute 'mode-line nil :background "#000000"
                      :foreground "#ffffff" :box 'unspecified)
  (add-hook 'after-make-frame-functions #'my-modus-re-enable-frame-theme))

(defun my-modus-theme-apply (choice)
  "Load the built-in Modus theme CHOICE without changing saved state."
  (setq choice (if (symbolp choice) choice (intern choice)))
  (unless (memq choice '(modus-vivendi modus-vivendi-tinted
                         modus-vivendi-deuteranopia modus-vivendi-tritanopia
                         modus-operandi modus-operandi-tinted
                         modus-operandi-deuteranopia modus-operandi-tritanopia))
    (user-error "Unknown Modus theme: %s" choice))
  (setq my-modus-theme choice)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme choice t)
  choice)

(defun my-modus-theme-set (choice)
  "Load and persist the built-in Modus theme CHOICE."
  (setq choice (my-modus-theme-apply choice))
  (customize-save-variable 'my-modus-theme choice))

(defun my-modus-theme-select ()
  "Select and persist a built-in Modus theme."
  (interactive)
  (let* ((themes '(modus-operandi-tinted modus-vivendi-tinted
                   modus-operandi modus-vivendi
                   modus-operandi-deuteranopia modus-operandi-tritanopia
                   modus-vivendi-deuteranopia modus-vivendi-tritanopia))
         (choice (intern (completing-read "Modus theme: " themes nil t))))
    (my-modus-theme-set choice)))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;;; early-init.el ends here
