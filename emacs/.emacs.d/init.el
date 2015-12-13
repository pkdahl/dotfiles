;;; init.el

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt)
       "USERNAME"
     "USER")))
(defvar base-dir (concat user-emacs-directory "base/"))
(defvar modules-dir (concat user-emacs-directory "modules/"))
(defvar personal-dir (concat user-emacs-directory "personal/"))
(defvar themes-dir (concat user-emacs-directory "themes/"))
(defvar data-dir (concat user-emacs-directory "data/")
  "Directory for auxiliary emacs files.")
(unless (file-directory-p data-dir)
  (make-directory data-dir))
(defvar cache-dir (concat user-emacs-directory "cache/")
  "Directory for temporary emacs files.")
(unless (file-directory-p cache-dir)
  (make-directory cache-dir))

;; Setup load-path

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path base-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path personal-dir)
(add-to-list 'load-path themes-dir)

;; Load core

(require 'init-bootstrap)
(require 'init-packages)
(require 'init-base)
(require 'init-ui)
(require 'init-keybindings)
(when (eq system-type 'darwin)
  (require 'init-osx))

;; Load modules

(dolist (module '(init-ace-window
		  init-avy
		  ;; init-clojure
		  ;; init-company
		  init-flycheck
		  init-helm
		  init-hungry-delete
		  ;; init-hydra
		  ;; init-javascript
		  ;; init-ledger
		  ;; init-notmuch
		  init-org
		  init-paradox
		  init-paredit
		  init-projectile
		  init-python
		  init-swiper
		  ;; init-tex
		  init-vc))
  (require module))

;; Load personal

(dolist (config '(calendar-config
		  mail-config))
  (require config))

(req-package org-config
  :require init-org)

(req-package-finish)

(setq custom-file (concat personal-dir "custom.el"))
(load custom-file 'noerror)
