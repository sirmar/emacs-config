;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Performance

(defun marcus-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time (time-subtract after-init-time before-init-time)))
    gcs-done))

(add-hook 'emacs-startup-hook #'marcus-display-startup-time)

;;; Package handling

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)
(use-package exec-path-from-shell)

;;; Core

(use-package emacs
  :ensure nil
  :custom
  (gc-cons-threshold (* 100 1000 1000))
  (read-process-output-max (* 1024 1024))
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ring-bell-function 'ignore)
  (frame-title-format "%b")
  (display-time-24hr-format t)
  (mouse-yank-at-point t)
  (make-backup-files nil)
  (auto-save-default nil)
  (user-full-name "Marcus Veibäck")
  (user-mail-address "sirmar@gmail.com")
  (mac-option-key-is-meta nil)
  (mac-command-key-is-meta t)
  (mac-command-modifier 'meta)
  (mac-option-modifier nil)
  (redisplay-dont-pause t)
  (scroll-margin 1)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position 1)
  (nxml-child-indent 4)
  (nxml-attribute-indent 4)
  (indent-tabs-mode nil)
  (tab-width 4)
  :config
  (display-time-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (when window-system (set-frame-position (selected-frame) 5 30))
  (when window-system (set-frame-size (selected-frame) 210 80))
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (line-number-mode t)
  (column-number-mode t)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (global-whitespace-mode t)
  (diminish 'global-whitespace-mode)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (eval-after-load "term"
    '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))
  :hook
  ((text-mode . (lambda () (setq whitespace-style '(face tabs trailing))))
   (prog-mode . (lambda () (setq whitespace-style '(face tabs trailing))))
   (go-mode   . (lambda () (setq whitespace-style '(face trailing))))
   (prog-mode . display-fill-column-indicator-mode))
  :init
  (setq display-fill-column-indicator-column 120))

;;; UI

(use-package zenburn-theme
  :init (load-theme 'zenburn t)
  :config (set-face-attribute 'region nil :background "#522"))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 130)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-time nil)
  (display-time-default-load-average nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 50))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Completion stack

(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode 1))

(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-c b"   . consult-project-buffer)
   ("C-c g"   . consult-ripgrep)))

;;; Auto-complete

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  :init (global-corfu-mode 1))

;;; Editing

(use-package multiple-cursors
  :bind (("C-+"   . mc/mark-next-like-this)
         ("C-M-+" . mc/mark-all-like-this)
         ("C-'"   . mc/edit-lines)))

(use-package move-text
  :bind (("<M-up>"   . move-text-up)
         ("<M-down>" . move-text-down)))

(use-package expand-region
  :bind (("M-SPC" . er/expand-region)))

(use-package wgrep)

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

(use-package crux
  :bind (("C-x C-k" . crux-delete-file-and-buffer)
         ("C-x C-r" . crux-rename-file-and-buffer)))

;;; Navigation

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config (custom-set-faces '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0))))))

(use-package magit
  :commands (magit-status magit-log-buffer-file)
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :init (global-diff-hl-mode 1)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;;; Dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-d" . dired-jump))
  :config
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches "-alGhvF --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
                  (define-key dired-mode-map (kbd ".") 'dired-up-directory)
                  (define-key dired-mode-map [mouse-2] 'dired-find-alternate-file))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config (define-key dired-mode-map "H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

;;; LSP

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  :hook
  ((python-ts-mode     . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode        . eglot-ensure)
   (js-ts-mode         . eglot-ensure)
   (bash-ts-mode       . eglot-ensure)
   (dockerfile-ts-mode . eglot-ensure)))

;;; Major modes

(setq treesit-language-source-alist
      '((python     "https://github.com/tree-sitter/tree-sitter-python")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")))

(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (csharp-mode     . csharp-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'"  . csharp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))

(use-package markdown-mode
  :commands (markdown-mode))

(use-package php-mode
  :commands (php-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(defun org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block-begin-line nil :background "#393939" :extend t)
  (set-face-attribute 'org-block-end-line nil :background "#393939" :extend t)
  (set-face-underline 'org-ellipsis nil)
  (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :bind (("C-c c" . org-capture))
  :config
  (setq org-startup-indented t
        org-default-notes-file "~/notes.org"
        org-todo-keywords '((sequence "TODO" "DOING" "DONE"))
        org-todo-keyword-faces '(("TODO" . "red") ("DOING" . "Orange") ("DONE" . "green"))
        org-log-done "time"
        org-src-fontify-natively t
        org-ellipsis "▾"
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)

  (org-font-setup))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;;; Hooks

(defun marcus-before-save-hook ()
  (delete-trailing-whitespace)
  (when (eq major-mode 'bash-ts-mode)
    (untabify (point-min) (point-max)))
  (when (eq major-mode 'restclient-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'marcus-before-save-hook)

;;; Custom functions

(defun marcus-kill-line-or-region ()
  "Cut region. If no region cut current line."
  (interactive)
  (if (use-region-p) (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(defun marcus-home ()
  "Move to indentation, beginning of line and beginning of buffer."
  (interactive)
  (if (bolp) (beginning-of-buffer)
    (skip-chars-backward " \t")
    (unless (bolp) (back-to-indentation))))

(defun marcus-end ()
  "Move to end of line and end of buffer."
  (interactive)
  (if (eolp) (end-of-buffer)
    (end-of-line)))

(defun marcus-comment ()
  "Comment eclipse style."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(defun marcus-goto-last-edit-point ()
  "Sets the cursor on the last edit point."
  (interactive)
  (let ((undos buffer-undo-list))
    (if (listp undos)
        (while (and undos
                    (let ((pos (or (cdr-safe (car undos)) (car undos))))
                      (not (and (integerp pos) (goto-char (abs pos))))))
          (setq undos (cdr undos))))))

;;; Key bindings

(bind-key "C-z"       'undo)
(bind-key "C-x C-z"   'undo)
(bind-key "<delete>"  'delete-char)
(bind-key "C-j"       (lambda () (interactive) (join-line -1)))
(bind-key "C-w"       'marcus-kill-line-or-region)
(bind-key "C-a"       'marcus-home)
(bind-key "C-e"       'marcus-end)
(bind-key "M-g"       'goto-line)
(bind-key "C-x C-b"   'ibuffer)
(bind-key "M-C-c"     'marcus-comment)
(bind-key "M-C-SPC"   'marcus-goto-last-edit-point)
(bind-key "S-SPC"     'cycle-spacing)
(bind-key "C-c s"     (lambda () (interactive) (ansi-term "/bin/bash")))
(bind-key "<f10>"     'next-match)

;;; Dependency checks

(defun marcus-check-dependencies ()
  (let ((warnings '()))
    (unless (find-font (font-spec :name "all-the-icons"))
      (push "all-the-icons fonts missing — run M-x all-the-icons-install-fonts" warnings))
    (dolist (lang '(python typescript tsx javascript yaml dockerfile bash c-sharp))
      (unless (treesit-language-available-p lang)
        (push (format "tree-sitter grammar missing: %s — run M-x treesit-install-language-grammar" lang) warnings)))
    (unless (executable-find "rg")
      (push "ripgrep missing — run: brew install ripgrep" warnings))
    (unless (executable-find "gls")
      (push "gls missing — run: brew install coreutils" warnings))
    (dolist (server '(("basedpyright-langserver"  . "Python")
                      ("typescript-language-server" . "TypeScript")
                      ("bash-language-server"    . "Bash")
                      ("docker-langserver"       . "Dockerfile")))
      (unless (executable-find (car server))
        (push (format "language server missing: %s (%s)" (car server) (cdr server)) warnings)))
    (when warnings
      (with-current-buffer (get-buffer-create "*Dependency warnings*")
        (erase-buffer)
        (insert "Missing dependencies:\n\n")
        (dolist (w (nreverse warnings))
          (insert "  • " w "\n"))
        (display-buffer (current-buffer))))))

(add-hook 'emacs-startup-hook #'marcus-check-dependencies)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
