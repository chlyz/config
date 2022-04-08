;;; lyzell-vc.el -*- lexical-binding; t: -*-
;; TODO: How to close the buffers when quitting?
;; TODO: How to get the diff from the merge base?

;; Install dependencies.
(straight-use-package 'magit)

;; Functions.
(defun lyzell/vc-grep ()
  "Grep for regexp in project."
  (interactive)
  (let* ((prefix "Search for (default ")
         (default (symbol-name (symbol-at-point)))
         (prompt (concat prefix default "): "))
         (pattern (read-regexp prompt))
         )
    (if (string= pattern "")
        (if (string= default "nil")
            (message "Abort: No pattern given!")
          (vc-git-grep default "\\\* .\\\*" (vc-root-dir)))
      (vc-git-grep pattern "\\\* .\\\*" (vc-root-dir)))))

;; Setup magit.
;; (require 'magit)
;; (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(provide 'lyzell-vc)
