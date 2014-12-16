;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;; Completion
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "<C-S-iso-lefttab>") 'dabbrev-completion)
(define-key minibuffer-local-map (kbd "<C-S-iso-lefttab>") 'dabbrev-completion)

;; Pairing
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; Macros
(global-set-key (kbd "<f12>") 'call-last-kbd-macro)

;; SVN
(global-set-key (kbd "C-c d") 'svn-file-show-svn-ediff)
(global-set-key (kbd "C-c r") 'vc-revert)
(global-set-key (kbd "C-c l") 'vc-print-log)
(global-set-key (kbd "C-c i") 'vc-next-action)
(global-set-key (kbd "C-c s") 'svn-examine)

;; Compilation
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'kill-compilation)
(global-set-key (kbd "<f9>") 'previous-error)
(global-set-key (kbd "<f10>") 'next-error)

;; Multiple markers
(key-chord-define-global (kbd "ee") 'mc/edit-lines)
(global-set-key (kbd "C-f") 'mc/mark-next-like-this)
(global-set-key (kbd "C-b") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)

;; Expand region
(global-set-key (kbd "C-n") 'er/expand-region)
(global-set-key (kbd "C-p") 'er/contract-region)

;; Key Chords
(key-chord-define-global (kbd "aa") 'rgrep)

;; Jedi
(global-set-key (kbd "M-.") 'jedi:goto-definition)
(global-set-key (kbd "M-:") 'jedi:goto-definition-pop-marker)

;; Ace
(key-chord-define-global (kbd ",,") 'ace-jump-word-mode)

;; Projectile
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-x s") 'projectile-switch-project)
(global-set-key (kbd "C-x g") 'projectile-grep)
(global-set-key (kbd "C-x r") 'projectile-replace)
(global-set-key (kbd "C-x t") 'projectile-toggle-between-implementation-and-test)

;; Misc
(global-set-key (kbd "C-z") 'undo)

