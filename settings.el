;; Buffer visulaization
(line-number-mode t)
(column-number-mode t)
(setq frame-title-format "%b")
(setq ediff-split-window-function (quote split-window-horizontally))

;; Whitespace
(global-whitespace-mode t)
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs trailing lines-tail indentation::space))
(add-hook 'before-save-hook 'sanitize-whitespace)

;; Search/Replace
(setq case-fold-search t)
(setq case-replace t)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)
(delete-selection-mode)

;; Org mode
(setq org-startup-indented t)
(setq org-startup-truncated nil)
(setq org-src-fontify-natively t)
(add-hook 'text-mode-hook
          (lambda ()
            (setq org-default-notes-file (concat org-directory "/todo.org"))
            (define-key global-map "\C-cc" 'org-capture)
            ))
(defface org-block-begin-line
  '((t (:background "#DDDDDD")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#EEEEEE")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:foreground "#111111" :background "#DDDDDD")))
  "Face used for the line delimiting the end of source blocks.")

;; Misc
(setq compile-command "")
(setq tags-file-name "~/TAGS")
(setq mouse-yank-at-point t)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
