;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flx-ido-mode
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Yasnippet
(yas-global-mode t)

;; Projectile
(projectile-global-mode)

;; Auto Complete
(ac-config-default)
(setq ac-ignore-case nil)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(setq ac-auto-show-menu 0.1)

;; Key Chords
(key-chord-mode 1)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

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
(setq make-backup-files nil)
(setq skeleton-pair t)
(setq mouse-yank-at-point t)
(defalias 'yes-or-no-p 'y-or-n-p)
