(load "~/.emacs.d/pre-cask.el")

(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)

(load "~/.emacs.d/cask-inits.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/key-bindings.el")
(load "~/.emacs.d/grep-settings.el")
