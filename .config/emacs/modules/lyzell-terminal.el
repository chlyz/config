;;; lyzell-vc.el -*- lexical-binding; t: -*-

(require 'eshell)
(require 'esh-mode)
(require 'esh-module)
(setq eshell-modules-list             ; It works but may need review
      '(eshell-alias
        eshell-basic
        eshell-cmpl
        eshell-dirs
        eshell-glob
        eshell-hist
        eshell-ls
        eshell-pred
        eshell-prompt
        eshell-script
        eshell-term
        ;; eshell-tramp
        eshell-unix))

;; (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'

(require 'em-cmpl)
(require 'em-dirs)
(setq eshell-cd-on-directory t)

(require 'em-tramp)
;; (setq eshell-prefer-lisp-functions t)
(setq password-cache t)
(setq password-cache-expiry 600)

(require 'em-hist)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)

;;; Keybindings

;;;; VTerm keybingings

(with-eval-after-load 'vterm
  '(evil-define-key 'normal vterm-mode-map (kbd "_") 'vterm-send-C-a)
  '(evil-define-key 'normal vterm-mode-map (kbd "0") 'vterm-send-C-a))

(eval-after-load 'vterm
  '(evil-define-key 'normal vterm-mode-map (kbd "_") 'vterm-send-C-a))
(eval-after-load 'vterm
  '(evil-define-key 'normal vterm-mode-map (kbd "0") 'vterm-send-C-a))
(eval-after-load 'vterm
  '(evil-define-key 'normal vterm-mode-map (kbd "gk") 'vterm-previous-prompt))

;; '(evil-define-key 'normal vterm-mode-map (kbd "0") 'vterm-send-C-a))

;;;; Eshell keybindings

(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-SPC") 'evil-send-leader))
(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-u") 'eshell-kill-input))
(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input))
(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input))

(defun lyzell-eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))


(require 'simple)
(defun lyzell-eshell-clear ()
  "Clear and such."
  (interactive)
  (eshell/clear)
  (evil-scroll-line-to-top (line-number-at-pos)))

(add-hook 'eshell-mode-hook
          (defun lyzell-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)))

(evil-define-key 'normal 'global (kbd "<leader>as") 'lyzell-eshell-other-window)
(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-l") 'lyzell-eshell-clear))
(eval-after-load 'eshell
  '(evil-define-key 'insert eshell-mode-map (kbd "C-l") 'lyzell-eshell-clear))
(eval-after-load 'eshell
  '(evil-define-key 'normal eshell-mode-map (kbd "C-l") 'lyzell-eshell-clear))

;;; Environmental variables

(setenv "SCONSFLAGS" "COMPILER=gcc DEVENV=gcc BITS=64 DEVENV_BITS=64 VERBOSE=false HOST=true MYSTIFY=0 -j12")
(setenv "NIRARC_SOURCED" "1")
(setenv "NIRA_NIRATOOLS" "/home/chlyz/git/tools/NIRATools")
(setenv "NIRA_JENKINSTOOLS" "/home/chlyz/git/tools/JenkinsTools")
(setenv "BOSCH_COMPILER_X52" " ")
(setenv "CMOCK_DIR" "/home/chlyz/git/tools/NIRATools/cmock/cmock_afa2949_patched")
(setenv "COAN" "/usr/bin")
(setenv "CPPUTEST_HOME" "/home/chlyz/git/tools/NIRATools/cpputest/3.7.1")
(setenv "GCC_V850_COMPILER" "/opt/cross/")
(setenv "GCC_X86_COMPILER" " ")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "M2RSM" "/home/chlyz/git/tools/NIRATools/MSquared/M2RSM")
(setenv "MATLAB_INCLUDES_32BIT" "/home/chlyz/git/tools/NIRATools/Matlab/includes/R2007b")
(setenv "MATLAB_INCLUDES_64BIT" "/home/chlyz/git/tools/NIRATools/Matlab/includes/R2020a")
(setenv "MW_MINGW64_LOC" " ")
(setenv "PCLint" "/home/chlyz/git/tools/NIRATools/pclint/9.00L")
(setenv "PUGIXML" "/home/chlyz/git/tools/NIRATools/Compilers/Libraries/pugixml/1.7")
(setenv "UNITY_DIR" "/home/chlyz/git/tools/NIRATools/cmock/cmock_afa2949_patched/vendor/unity")
(setenv "JAVA_HOME" "/usr/lib/jvm/java-11-openjdk-amd64")

(provide 'lyzell-terminal)
