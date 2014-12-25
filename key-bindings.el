;; Buffers
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-,") 'back-window)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-M-,") 'previous-buffer)
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "<f8>") 'config-buffers)

;; Edit
(global-set-key (kbd "C-c SPC") 'goto-last-edit-point)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "<delete>") 'delete-char)

;; Move cursor
(global-set-key (kbd "<down>") 'next-logical-line)
(global-set-key (kbd "<up>") 'previous-logical-line)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

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

;; Multiple markers
(key-chord-define-global (kbd "qq") 'mc/edit-lines)
(global-set-key (kbd "C-f") 'mc/mark-next-like-this)
(global-set-key (kbd "C-b") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)

;; Expand region
(global-set-key (kbd "C-n") 'er/expand-region)

;; Jedi
(global-set-key (kbd "M-.") 'jedi:goto-definition)
(global-set-key (kbd "M-:") 'jedi:goto-definition-pop-marker)

;; Search
(global-set-key (kbd "C-c C-s") 'ace-jump-word-mode)
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
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<f12>") 'call-last-kbd-macro)
