;; ~/.emacs.d/init.el

;;; Initialization

(setq inhibit-startup-message t)

(defvar current-user (getenv (if (equal system-type 'windows-nt)
                 "USERNAME"
                 "USER")))

(defvar config-dir (file-name-directory load-file-name)
  "Directory for Emacs configuration files.")

(defvar cache-dir "~/.cache/emacs/"
  "Directory for temporary Emacs files.")

(unless (file-directory-p cache-dir)
  (make-directory cache-dir t))

(defvar data-dir "~/.local/share/emacs/"
  "Directory for auxiliary Emacs files.")

(unless (file-directory-p data-dir)
  (make-directory data-dir t))

;;;; Setup `package'

(require 'package)
(setq package-enable-at-startup nil
  package-archives
    '(("gnu"          . "https://elpa.gnu.org/packages/")
      ("melpa-stable" . "https://stable.melpa.org/packages/")
      ("melpa"        . "https://melpa.org/packages/")
      ("org"          . "http://orgmode.org/elpa/"))
  package-pinned-packages nil
  package-user-dir (concat data-dir "site-lisp")
  package-archive-priorities
    '(("melpa-stable" . 10)
      ("gnu"          . 5)
      ("melpa"        . 0)))

(when (version< emacs-version "26.3")
  (setq package-check-signature nil
  gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

;;;; Bootstrap `use-package' from Melpa stable

(unless (package-installed-p 'use-package)
  (let ((package-archives
          '(("melpa-stable" . "https://stable.melpa.org/packages/"))))
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

; (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
; (add-to-list 'load-path config-dir)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :pin melpa-stable
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

(setq default-frame-alist '((width  . 120)
                            (height . 50))
  split-width-threshold 100)

;;;; Mode Line

(column-number-mode 1)

;;;; Minibuffer > Savehist

(use-package savehist
  :config
  (setq savehist-file (concat cache-dir "history")))

;;;; I18n

(set-language-environment "UTF-8")

;;; UI

;;;; Faces

;; (when (member "Source Code Pro" (font-family-list))
;;   (set-default-font "Source Code Pro"))

;; (when (member "Fira Code" (font-family-list))
;;   (set-default-font "Fira Code"))

(when (member "DejaVu Sans Mono" (font-family-list))
  (set-default-font "DejaVu Sans Mono"))

;; (when (member "mononoki Nerd Font" (font-family-list))
;;   (set-default-font "mononoki Nerd Font"))

;;;; Theme

; (use-package color-theme-sanityinc-solarized
;   :disabled t
;   :pin melpa-stable
;   :init
;   (when (member "Fira Code" (font-family-list))
;	(set-default-font "Fira Code"))
;   (load-theme 'sanityinc-solarized-dark t))

; (use-package dracula-theme
;   :disabled t
;   :pin melpa-stable
;   :init
;   (when (member "Fira Code" (font-family-list))
;	(set-default-font "Fira Code"))
;   (load-theme 'dracula t))

; (use-package leuven
;   :disabled t
;   :pin melpa-stable
;   :ensure leuven-theme
;   :init
;   (setq leuven-scale-outline-headlines nil
;		leuven-scale-org-agenda-structure nil)
;   (load-theme 'leuven t))

; (use-package zenburn-theme
;   :disabled t
;   :pin melpa-stable
;   :init
;   (when (member "Fira Code" (font-family-list))
;	(set-default-font "Fira Code"))
;   (load-theme 'zenburn t))

(use-package nord-theme
  :ensure t
  :pin melpa-stable
  :init
  (when (member "Fira Code" (font-family-list))
  (set-default-font "Fira Code"))
  (load-theme 'nord t))

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
        ns-right-option-modifier 'nil))

;;; Help

;;;; which-key

; (unless (string< emacs-version "24.4")
;   (use-package which-key
;	:pin melpa-stable
;	:ensure t
;	:diminish which-key-mode
;	:config
;	(which-key-mode)
;	(setq which-key-idle-delay 0.5)))

;;; Convenience

;;;; Abbreviations

(setq abbrev-file-name (concat config-dir "abbrev_defs")
  save-abbrevs t)

;;;; Register

; (use-package register-setup
;   :after register)

;;;; ibuffer

(bind-key "C-x B" 'ibuffer)

;;;; ivy, counsel & avy

; (use-package ivy
;   :ensure t
;   :diminish (ivy-mode . "")
;   :bind ("C-x b" . ivy-switch-buffer)
;   :config
;   (ivy-mode 1)
;   (setq ivy-use-virtual-buffers t))

; (use-package counsel
;   :ensure t
;   :bind* (("M-x"     . counsel-M-x)
;     ("C-x C-f" . counsel-find-file))
;   :config
;   (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

; (use-package avy
;   :bind (("M-g l n" . avy-goto-line-below)
;		 ("M-g l p" . avy-goto-line-above)
;		 ("M-g l c" . avy-goto-char-in-line)
;		 ("M-g w"   . avy-goto-word-or-subword-1)))

;;;; RYO modal

; (use-package ryo-modal
;   :ensure t
;   :commands ryo-modal-mode
;   :bind ("C-z" . ryo-modal-mode)
;   :config
;   (ryo-modal-key
;    "m" '(("." ryo-modal-repeat)
;		 ("h" backward-char)
;		 ("j" next-line)
;		 ("k" previous-line)
;		 ("l" forward-char)
;		 ("q" ryo-modal-mode))))

; (use-package hydra
;   :ensure t
;   :config
;   (setq hydra-is-helpful t)

;   (defhydra hydra-move
;	(:color pink :hint nil)
; "
; _n_ext line      _a_: beg of line  _<_: top
; _p_rev line      _e_: end of line  _>_: bottom
; _f_orward char   _[_: scroll up    _l_: recenter
; _b_ackward char  _]_: scroll down  _q_: quit
; "
;	("n" next-line)
;	("p" previous-line)
;	("f" forward-char)
;	("b" backward-char)
;	("a" beginning-of-line)
;	("e" move-end-of-line)
;	("[" scroll-up-command)
;	("]" scroll-down-command)
;	("l" recenter-top-bottom)
;	("<" beginning-of-buffer)
;	(">" end-of-buffer)

;	("jl" goto-line)
;	("jw" avy-goto-word-or-subword-1)

;	("t" (if (equal linum-mode t) (linum-mode -1) (linum-mode t))
;	 "toggle linum-mode")

;	("q" nil)
;	("ESC" nil)))

  ;; define other hydras
  ;; goto line
  ;; jump to
  ;; add mark stuff...

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

;; package is newcomment.el
(bind-key "C-," 'comment-line)
(bind-key "M-," 'comment-or-uncomment-region)

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

; (use-package swiper
;   ;; :pin melpa-stable
;   :ensure t
;   :bind ("C-s" . swiper))

;;;; Smartparens

;; https://ebzzry.github.io/emacs-pairs.html

; (use-package smartparens
;   :disabled t
;   :pin melpa-stable
;   :after smartparens-config
;   :init
;   (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
;   (add-hook 'scheme-mode-hook #'smartparens-mode)
;   (bind-keys :map smartparens-mode-map
;			 ("C-M-a"     . sp-beginning-of-sexp)
;			 ("C-M-e"     . sp-end-of-sexp)

;			 ("C-<down>"  . sp-down-sexp)
;			 ("C-<up>"    . sp-up-sexp)
;			 ("M-<down>"  . sp-backward-down-sexp)
;			 ("M-<up>"    . sp-backward-up-sexp)

;			 ("C-<right>" . sp-forward-slurp-sexp)
;			 ("M-<right>" . sp-forward-barf-sexp)
;			 ("C-<left>"  . sp-backward-slurp-sexp)
;			 ("M-<right>" . sp-backward-barf-sexp)

;			 ("C-M-d"     . delete-sexp)))

;;; Communication

(setq url-configuration-directory (concat config-dir "url/"))

;;; Games

(setq tetris-score-file (concat cache-dir "tetris-scores"))

;;; Mail

; (use-package mu4e
;   ;; provided by mu
;   :if (executable-find "mu")
;   :ensure nil
;   :bind ("<f12>" . mu4e)
;   :init
;   (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
;		mu4e-headers-fields '( (:date       . 17)
;                (:flags      . 5)
;                (:from-or-to . 25)
;                (:subject    . nil)))
;   (setq message-send-mail-function 'message-send-mail-with-sendmail
;		sendmail-program (executable-find "msmtpq")
;		message-kill-buffer-on-exit t
;		mu4e-change-filenames-when-moving t
;		;; don't retrieve mail with mu4e
;		mu4e-get-mail-command "true")
;   :config
;   ;; don't apply trashed flag, just move
;   ;; FIX we get code for trash twice in the list
;   (add-to-list 'mu4e-marks
;        '(trash :char ("d" . "▼")
;            :prompt "dtrash"
;            :dyn-target (lambda (target msg)
;									 (mu4e-get-trash-folder msg))
;            :action (lambda (docid msg target)
;								 (mu4e~proc-move
;                 docid
;                 (mu4e~mark-check-target target)
;                 "-N")))))

; (use-package org-mu4e
;   :after (mu4e org-mode))

; (use-package mu4e-setup
;   :after (mu4e))

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

;;;;; Coq

; (use-package proof-site
;   :if (package-installed-p 'proof-general)
;   :mode ("\\.v\\'" . coq-mode)
;   :init (when (package-installed-p 'proof-general)
;     (add-to-list 'load-path (let ((cmd (concat "printf %s \"$(find " package-user-dir " -name generic)\"")))
;									(shell-command-to-string cmd)))))

;;;;; Elm

; (use-package elm-mode
;   :pin melpa-stable
;   :mode "\\.elm\\'")

;;;;; Maude

; (use-package maude-mode
;   :mode "\\.maude\\'")

;;;;; Haskell

; (use-package haskell-mode
;   :pin melpa-stable
;   :mode "\\.hs\\'"
;   :init
;   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;   (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;;;;; OCaml

; (defvar opam-share
;   (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;	(when (and opam-share (file-directory-p opam-share))
;   ;; Register Merlin
;   (let ((opam-lisp (expand-file-name "emacs/site-lisp" opam-share)))
;		(unless (member opam-lisp load-path)
;     (add-to-list 'load-path opam-lisp)))
;   opam-share))
;   "opam share directory")

; (use-package tuareg
;   :if opam-share
;   :ensure nil
;   :mode (("\\.ml[ilpy]?$" . tuareg-mode)
;		 ("\\.topml$" . tuareg-mode)))

; (use-package merlin
;   :if opam-share
;   :ensure nil
;   :config
;   (add-hook 'tuareg-mode-hook #'merlin-mode)
;   (add-hook 'caml-mode-hook #'merlin-mode)
;   (setq merlin-command 'opam
;		merlin-use-auto-complete-mode t
;		merlin-error-after-save nil))

; (use-package ocamlformat
;   :if opam-share
;   :config
;   (add-hook 'tuareg-mode-hook
;			(lambda ()
;       (define merlin-mode-map (kbd "C-M-<tab>") 'ocamlformat)
;       (add-hook 'before-save-hook 'ocamlformat-before-save))))

; (use-package utop
;   :if opam-share
;   :ensure nil
;   :config
;   (add-hook 'tuareg-mode-hook #'utop-minor-mode)
;   (if (executable-find "opam")
;   (setq utop-command "opam config exec -- utop -emacs")))

;;;;; ReasonML

;; https://github.com/reasonml-editor/reason-mode

; (use-package reason-mode
;   :pin melpa-stable
;   :mode ("\\.re\\'" "\\.rei\\'"))

;;;;; Python

; (use-package elpy
;   :pin melpa-stable
;   :init
;   (setq elpy-rpc-python-command "python3"
;		python-shell-interpreter "python3")
;   :config
;   (elpy-enable))

;;;;; Scheme

; (use-package geiser
;   :pin melpa-stable
;   :config
;   (setq geiser-active-implementations '(guile)))

;;; Text

;;;; Outlines

;;;;; Org

; (use-package org

;   :bind
;   (("C-c l"   . org-store-link)
;    ("C-c C-l" . org-insert-link))

;   :preface
;   (defface org-todo-keyword-waiting-face
;	'((((class color))
;    (:foreground "orange" :weight bold))
;   (t (:weight bold)))
;	"face to fontify Org TODO keyword WAITING"
;	:group 'org)
;   (defface org-todo-keyword-cancelled-face
;	'((((class color))
;    (:foreground "yellow" :weight bold))
;   (t (:weight bold)))
;	"face to fontify Org TODO keyword CANCELLED"
;	:group 'org)

;   :config
;   (setq org-archive-location "archive/%s_archive::"
;		org-hide-emphasis-markers t
;		org-hide-leading-stars t
;		org-log-done 'time
;		org-log-into-drawer t
;		org-startup-indented t
;		org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
;							(sequence "WAITING(w)" "|" "CANCELLED(c)"))
;		org-todo-keyword-faces '(("WAITING" . org-todo-keyword-waiting-face)
;								 ("CANCELLED" . org-todo-keyword-cancelled-face)))
;   (add-hook 'org-mode-hook
;		(lambda ()
;     (add-hook 'before-save-hook
;			(lambda ()
;       (whitespace-cleanup))))))

; (use-package org-mac-link
;   :if (string-equal system-type "darwin")
;   :after (org)
;   :bind ("C-c m" . org-mac-grab-link))

; (defun time-string-days-ago (n)
;   "Description"
;   (format-time-string "[%Y-%m-%d]"
;           (time-subtract (current-time) (days-to-time n))))

; (use-package org-capture
;   :after (org)
;   :demand t
;   :bind
;   ("C-c c" . org-capture))

; (use-package org-habit
;   :preface (add-to-list 'org-modules 'org-habit)
;   :after (org))

; (use-package org-id
;   :after (org)
;   :config
;   (setq org-id-locations-file (concat cache-dir "org-id-location")))

; (use-package org-setup
;   :after (org))

;;;;; Outshine

;; Enable org-mode-like folding in emacs-lisp-mode

; (use-package outshine
;   :if (> emacs-major-version 24)
;   :pin melpa-stable
;   :ensure t
;   :diminish outline-minor-mode
;   :init
;   (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;   (add-hook 'ruby-mode-hook 'outline-minor-mode)
;   :config
;   (setq outshine-startup-folded-p t
;		outshine-use-speed-commands t))

;;;; Markdown

; (use-package markdown-mode
;   :pin melpa-stable
;   :mode
;   (("README\\.md\\'" . gfm-mode)
;    ("\\.md\\'"       . markdown-mode)
;    ("\\.markdown\\'" . markdown-mode))
;   :init
;   (add-hook 'markdown-mode-hook
;		(lambda ()
;     (auto-fill-mode 0)
;     (visual-line-mode 1))))

(setq display-line-numbers 'visual)

(use-package evil
  :ensure t
  :pin melpa-stable
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :pin melpa-stable
  :after evil
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode 1))

(use-package magit
  :ensure t
  :pin melpa-stable
  :after evil
  :init (define-key evil-normal-state-map (kbd "vg") 'magit-status)
  :custom-face
  (magit-diff-hunk-heading ((((type tty)) (:foreground "blue" :background "white"))))
  (magit-diff-hunk-heading-highlight ((((type tty)) (:foreground "white" :background "blue"))))
  (magit-diff-hunk-heading-selection ((((type tty)) (:foreground "yellow" :background "blue"))))
  (magit-diff-added ((((type tty)) (:foreground "green"))))
  (magit-diff-added-highlight ((((type tty)) (:foreground "green" :background "brightblack"))))
  (magit-diff-removed ((((type tty)) (:foreground "red"))))
  (magit-diff-removed-highlight ((((type tty)) (:foreground "red" :background "brightblack"))))
  (magit-diff-context-highlight ((((type tty)) (:foreground "white" :background "brightblack"))))
  (magit-context-highlight ((((type tty)) (:background "blue"))))
  (magit-section-highlight ((((type tty)) (:background "blue")))))

(use-package evil-magit
  :ensure t
  :pin melpa-stable
  :after (evil magit))

(use-package editorconfig
  :ensure t
  :pin melpa-stable
  :config (editorconfig-mode 1))

(use-package org
  :init
  (setq org-adapt-indentation nil
        org-hide-leading-stars t
        org-log-into-drawer t
        org-todo-keywords
          '((sequence "TODO" "|" "DONE(!)"))
        org-startup-indented t))

; (use-package org-agenda

;   :after (org org-habit)
;   :bind ("C-c a" . org-agenda)
;   :config
;   (setq org-agenda-start-on-weekday nil
;		org-agenda-time-leading-zero t)
;   (setq org-agenda-custom-commands
;		'(("c" "Agenda"
;      ((tags "PRIORITY=\"A\""
;         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;          (org-agenda-overriding-header "High-priority tasks:")))
;			(agenda ""
;					((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
;			(tags-todo "-TODO=\"WAITING\"-PRIORITY=\"A\"-PRIORITY=\"C\""
;            ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
;						(org-agenda-overriding-header "Normal-priority tasks:")))))
;     ("r" "Archivable tasks"
;      ((tags (concat "CLOSED<\"" (time-string-days-ago 14) "\"")
;         ((org-agenda-overriding-header "Archivable tasks:")))
;			(tags (concat "TIMESTAMP<\"" (time-string-days-ago 14) "\"")
;         ((org-agenda-overriding-header "Archivable entries:"))))
;      ((org-agenda-compact-blocks t))))))

(use-package org-agenda
  :after org
  :bind (("C-c a" . org-agenda))
  :init
  (when (file-directory-p "~/Documents/houston")
    (add-to-list 'org-agenda-files "~/Documents/houston/"))
  (when (file-directory-p "~/Documents/notes")
    (add-to-list 'org-agenda-files "~/Documents/notes/"))
  :config
  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday nil
        org-agenda-time-leading-zero t

        org-agenda-custom-commands
          '(("c" "Agenda"
             ((agenda ""
                ((org-agenda-span 1)
                 (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'todo 'done))))
              (tags-todo "-PRIORITY=\"C\""
                ((org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'scheduled 'deadline))
                 (org-agenda-overrriding-header "Unscheduled tasks"))))))))

(use-package evil-org
  :ensure t
  :pin melpa-stable
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
    (lambda ()
      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :init
  (evil-define-key
    '(normal visual)
    'evil-org-mode (kbd "C-i") 'org-cycle))

;;; Customize

; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file (concat config-dir  "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file 'noerror)
