;; Clean emacs interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; Init cask packages. See "Cask" file.
(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)

;; Load my settings
(load "~/.emacs.d/variables.el")
(load "~/.emacs.d/grep-settings.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/keys.el")
