;; Buffer visulaization
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(setq frame-title-format "%b")

;; Whitespace
(setq whitespace-line-column 100)
(setq whitespace-style '(empty tabs trailing lines-tail))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'sanitize-whitespace)

;; Search/Replace
(setq case-fold-search t)
(setq case-replace t)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)
(delete-selection-mode)

;; Misc
(setq compile-command "")
(setq tags-file-name "~/TAGS")
(setq make-backup-files nil)
(electric-pair-mode t)
(setq mouse-yank-at-point t)
(defalias 'yes-or-no-p 'y-or-n-p)
