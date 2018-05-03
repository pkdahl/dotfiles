;; ~/.emacs.d/init.el

;;; Initialization

(setq inhibit-startup-message t)

(defvar current-user (getenv (if (equal system-type 'windows-nt)
								 "USERNAME"
							   "USER")))

(defvar config-dir (concat user-emacs-directory "settings/")
  "Directory for emacs configuration files.")

(defvar cache-dir "~/.cache/emacs/"
  "Directory for temporary emacs files.")

(unless (file-directory-p cache-dir)
  (make-directory cache-dir t))

(defvar data-dir "~/.local/share/emacs/"
  "Directory for auxiliary emacs files.")

(unless (file-directory-p data-dir)
  (make-directory data-dir t))

;;;; Setup `package'

(require 'package)
(setq package-enable-at-startup nil
	  package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("melpa"        . "https://melpa.org/packages/")
						 ("org"          . "http://orgmode.org/elpa/"))
	  package-pinned-packages nil
	  package-user-dir (concat data-dir "site-lisp")
	  package-archive-priorities '(("melpa-stable" . 10)
								   ("gnu"          . 5)
								   ("melpa"        . 0)))

(package-initialize)

;;;; Bootstrap `use-package' from Melpa stable

(unless (package-installed-p 'use-package)
  (let ((package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/"))))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package))))

(eval-when-compile
  (require 'use-package))

;;;; Setup `load-path'

;; Add Homebrew site-lisp direcotry to load-path if present
(let ((default-directory  "/usr/local/share/emacs/site-lisp/"))
  (when (file-directory-p default-directory)
	(normal-top-level-add-subdirs-to-load-path)))

;; Add packages provided by Nix to load-path
(let ((default-directory "/run/current-system/sw/share/emacs/site-lisp/"))
  (when (file-directory-p default-directory)
	(normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path config-dir)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :pin melpa-stable
  :init (exec-path-from-shell-initialize))

;;; Environment

;;;; Frames and Windows

(dolist (mode '(blink-cursor-mode
				scroll-bar-mode
				tool-bar-mode))
  (when (fboundp mode)
	(funcall mode -1)))

;; Turn of menu-bar-mode too, unless we are on OS X
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))

(setq default-frame-alist '((width . 120)
							(height . 50))
	  split-width-threshold 100)

;;;; Mode Line

(column-number-mode 1)

;;;; I18n

(set-language-environment "UTF-8")

;;; UI

;;;; Faces

(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro"))

;;;; Theme

(use-package leuven
  :ensure leuven-theme
  :pin melpa-stable
  :init
  (setq leuven-scale-outline-headlines nil
		leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

;;;; OS X keys

;; - Need option key with Norwegian keyboard
;; - Use command key as =meta=
;; - fn-key is =hyper=
;; - Right command key is =super=
;; - Right option key is =alt=
(when (string-equal system-type "darwin")
  (setq ns-function-modifier 'hyper
		ns-option-modifier 'nil          ; ns-alternate-modifier
		ns-command-modifier 'meta        ; mac-command-modifier
		ns-right-command-modifier 'super ; mac-right-command-modifier
		ns-right-option-modifier 'alt))

;;; Help

;;;; which-key

(unless (string< emacs-version "24.4")
  (use-package which-key
	:ensure t
	:pin melpa-stable
	:diminish which-key-mode
	:config
	(which-key-mode)
	(setq which-key-idle-delay 0.5)))

;;; Convenience

;;;; Abbreviations

(setq abbrev-file-name (concat config-dir "abbrev_defs")
	  save-abbrevs t)

;;;; ibuffer

(bind-key "C-x B" 'ibuffer)

;;;; ivy and counsel

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind ("C-x b" . ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :bind* (("M-x"     . counsel-M-x)
	  ("C-x C-f" . counsel-find-file))
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

;;; Files

(use-package recentf
  :commands (recentf-mode
			 counsel-recentf)
  :config
  (setq recentf-save-file (concat cache-dir "recentf")
		recentf-max-menu-items 50
		recentf-exclude '("/.git/")))

(setq auto-save-list-file-prefix (concat cache-dir "auto-save-list/saves-"))

(setq backup-directory-alist `((".*" . ,(concat cache-dir "backups/"))))

(global-auto-revert-mode 1)

;;; External

;;;; EasyPG

(use-package epa
  :init
  (setq epa-pinentry-mode 'loopback))

;;; Editing

;; Turn on disabled functions
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(bind-key "M-;" 'comment-or-uncomment-region)

;;;; Bookmark

(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat cache-dir "bookmarks"))

;;;; Display

(setq-default truncate-lines t)

;;;; Fill

(setq sentence-end-double-space nil)
(setq-default fill-column 79)

;;;; Indent

;; Spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;;;; Matching

(show-paren-mode 1)

(use-package swiper
  ;; :pin melpa-stable
  :ensure t
  :bind ("C-s" . swiper))

;;;; Smartparens

;; https://ebzzry.github.io/emacs-pairs.html

(use-package smartparens
  :pin melpa-stable
  :after smartparens-config
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode)
  (bind-keys :map smartparens-mode-map
			 ("C-M-a"     . sp-beginning-of-sexp)
			 ("C-M-e"     . sp-end-of-sexp)

			 ("C-<down>"  . sp-down-sexp)
			 ("C-<up>"    . sp-up-sexp)
			 ("M-<down>"  . sp-backward-down-sexp)
			 ("M-<up>"    . sp-backward-up-sexp)

			 ("C-<right>" . sp-forward-slurp-sexp)
			 ("M-<right>" . sp-forward-barf-sexp)
			 ("C-<left>"  . sp-backward-slurp-sexp)
			 ("M-<right>" . sp-backward-barf-sexp)

			 ("C-M-d"     . delete-sexp)))

;;; Communication

(setq url-configuration-directory (concat cache-dir "url/"))

;;; Games

(setq tetris-score-file (concat cache-dir "tetris-scores"))

;;; Mail

(use-package mu4e
  ;; provided by mu
  :if (executable-find "mu")
  :ensure nil
  :bind ("<f12>" . mu4e)
  :init
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
		mu4e-headers-fields '( (:date       . 17)
							   (:flags      . 5)
							   (:from-or-to . 25)
							   (:subject    . nil)))
  (setq message-send-mail-function 'message-send-mail-with-sendmail
		sendmail-program (executable-find "msmtpq")
		message-kill-buffer-on-exit t
		mu4e-change-filenames-when-moving t
		;; don't retrieve mail with mu4e
		mu4e-get-mail-command "true")
  :config
  ;; don't apply trashed flag, just move
  ;; FIX we get code for trash twice in the list
  (add-to-list 'mu4e-marks
			   '(trash :char ("d" . "▼")
					   :prompt "dtrash"
					   :dyn-target (lambda (target msg)
									 (mu4e-get-trash-folder msg))
					   :action (lambda (docid msg target)
								 (mu4e~proc-move docid
								   (mu4e~mark-check-target target) "-N")))))

(use-package org-mu4e
  :after (mu4e org-mode))

(use-package mu4e-setup
  :after (mu4e))

;;; Multimedia

(setq image-dired-dir (concat cache-dir "image-dired/"))

;;; Programming

;;;; Languages

(use-package prog-mode
  :init
  (add-hook 'prog-mode-hook
			(lambda ()
			  (add-hook 'before-save-hook
						(lambda ()
						  (whitespace-cleanup))))))

;;;;; CC mode (C, C++, Java etc.)

(use-package cc-mode
  :init
  (setq-default c-basic-offset 4
				tab-width 4
				indent-tabs-mode t))

;;;;; Elm

(use-package elm-mode
  :pin melpa-stable
  :mode "\\.elm\\'")

;;;;; Haskell

(use-package haskell-mode
  :pin melpa-stable
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;;;;; OCaml

(use-package tuareg
  :pin melpa-stable
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
		 ("\\.topml$" . tuareg-mode))
  :config
  (add-to-list 'load-path (concat
						   (replace-regexp-in-string
							"\n$"
							""
							(shell-command-to-string
							 "opam config var share"))
						   "/emacs/site-lisp"))
  (require 'ocp-indent))

(use-package merlin
  :pin melpa-stable
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'caml-mode-hook #'merlin-mode)
  (setq merlin-command 'opam
		merlin-use-auto-complete-mode t
		merlin-error-after-save nil))

(use-package utop
  :pin melpa-stable
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (if (executable-find "opam")
	  (setq utop-command "opam config exec -- utop -emacs")))

;;;;; Python

(use-package elpy
  :pin melpa-stable
  :init
  (setq elpy-rpc-python-command "python3"
		python-shell-interpreter "python3")
  :config
  (elpy-enable))

;;;;; Scheme

(use-package geiser
  :pin melpa-stable
  :config
  (setq geiser-active-implementations '(guile)))

;;;; Tools

;;;;; Magit

(use-package magit
  :pin melpa-stable
  :bind ("C-x g" . magit-status))

;;; Text

;;;; Outlines

;;;;; Org

(use-package org
  :bind
  (("C-c l"   . org-store-link)
   ("C-c C-l" . org-insert-link))
  :config
  (setq	org-hide-emphasis-markers t
		org-hide-leading-stars t
		org-log-done 'time
		org-startup-indented t)
  (add-hook 'org-mode-hook
			(lambda ()
			  (add-hook 'before-save-hook
						(lambda ()
						  (whitespace-cleanup))))))

(use-package org-mac-link
  :if (string-equal system-type "darwin")
  :after (org)
  :bind ("C-c m" . org-mac-grab-link))

(use-package org-agenda
  :after (org)
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-start-on-weekday nil
		org-agenda-time-leading-zero t))

(use-package org-capture
  :after (org)
  :demand t
  :bind
  ("C-c c" . org-capture))

(use-package org-id
  :after (org)
  :config
  (setq org-id-locations-file (concat cache-dir "org-id-location")))

(use-package org-setup
  :after (org))

;;;;; Outshine

;; Enable org-mode-like folding in emacs-lisp-mode

;; (use-package outshine
;;   :pin melpa
;;   :ensure t
;;   :diminish outline-minor-mode
;;   :init
;;   (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;;   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;;   :config
;;   (setq outshine-startup-folded-p t
;;         outshine-use-speed-commands t))

;;;; Markdown

(use-package markdown-mode
  :pin melpa-stable
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook
			(lambda ()
			  (auto-fill-mode 0)
			  (visual-line-mode 1))))

;;; Customize

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;;; Finalize

(load custom-file 'noerror)
