;;; ~/.emacs.d/init.el

(load-file (concat user-emacs-directory "config/environment.el"))

(require 'sane-defaults) ; Turn off splash screen etc. immediately
(package-initialize)     ; Keep this to shut up Package.el
(require 'package-setup) ; Load this first
(require 'base-setup)
(require 'ui-setup)
(require 'osx-setup)
(require 'elm-setup)
(require 'haskell-setup)
(require 'markdown-setup)
(require 'magit-setup)
(require 'org-setup)
(require 'keybindings)

(req-package-finish)

(setq custom-file (concat config-dir "custom.el"))
(load custom-file 'noerror)
