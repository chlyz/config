;;; lyzell-org.el -*- lexical-binding; t: -*-

(require 'org)

(setq org-directory (convert-standard-filename "~/Documents/org"))
(setq org-imenu-depth 7)

;;;; general settings
(setq org-adapt-indentation nil)
;; (setq org-special-ctrl-a/e nil)
;; (setq org-special-ctrl-k nil)
(setq org-M-RET-may-split-line '((default . nil)))
(setq org-hide-emphasis-markers nil)
(setq org-hide-macro-markers nil)
;; (setq org-hide-leading-stars nil)
;; (setq org-cycle-separator-lines 0)

(customize-set-value 'org-log-into-drawer t)
(customize-set-value 'org-clock-into-drawer "CLOCKING")
(customize-set-value 'org-tags-column -78)
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "START(s)" "MEET(m)" "WAIT(w)" "IDEA(i)" "PROJECT(p)" "|" "DONE(d)" "DROP(c)")))
(evil-define-key 'normal 'org-mode-map (kbd "<leader>oh")  'consult-org-heading)
(evil-define-key 'normal 'org-mode-map (kbd "<leader>oa")  'org-agenda)

(add-hook 'org-mode-hook 'org-indent-mode)

(provide 'lyzell-org)
