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


