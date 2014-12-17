;; Clean emacs interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; Set window size
(when window-system (set-frame-size (selected-frame) 195 47))
(when window-system (set-frame-position (selected-frame) 20 40))

