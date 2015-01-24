;; Buffers
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-,") 'back-window)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-M-,") 'previous-buffer)
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "<f8>") 'config-buffers)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Edit
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "<C-return>") 'open-line-above)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "M-S-SPC") 'just-one-space)
(global-set-key (kbd "M-C-<backspace>") 'sp-backward-kill-sexp)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "C-w") 'kill-word-or-region)

;; Navigation
(global-set-key (kbd "<down>") 'next-logical-line)
(global-set-key (kbd "<up>") 'previous-logical-line)
(global-set-key (kbd "<C-M-up>") 'sp-previous-sexp)
(global-set-key (kbd "<C-M-down>") 'sp-next-sexp)
(global-set-key (kbd "<C-M-left>") 'sp-backward-up-sexp)
(global-set-key (kbd "<C-M-right>") 'sp-down-sexp)
(global-set-key (kbd "C-a") 'my-home)
(global-set-key (kbd "C-e") 'my-end)
(global-set-key (kbd "C-c SPC") 'goto-last-edit-point)
(global-set-key (kbd "M-g") 'goto-line)

;; Completion
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "<C-S-iso-lefttab>") 'dabbrev-completion)
(define-key minibuffer-local-map (kbd "<C-S-iso-lefttab>") 'dabbrev-completion)

;; Compilation
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'kill-compilation)
(global-set-key (kbd "<f9>") 'previous-error)
(global-set-key (kbd "<f10>") 'next-error)

;; Smart scan
(global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)

;; Expand region
(global-set-key (kbd "C-S-SPC") 'er/expand-region)

;; Jedi
(global-set-key (kbd "M-.") 'jedi:goto-definition)
(global-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)

;; Search
(global-set-key (kbd "C-f") 'ace-jump-word-mode)
(global-set-key (kbd "C-c g") 'rgrep)

;; Projectile
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-x s") 'projectile-switch-project)
(global-set-key (kbd "C-x g") 'projectile-grep)
(global-set-key (kbd "C-x q") 'projectile-replace)
(global-set-key (kbd "C-x t") 'projectile-toggle-between-implementation-and-test)

;; Magit
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-mode)

;; Misc
(global-set-key (kbd "<f12>") 'call-last-kbd-macro)
(global-set-key (kbd "C-z") 'undo)
