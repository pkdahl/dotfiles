;; ~/.emacs.d/init.el

;;; Initialization

(setq inhibit-startup-message t)

(defvar current-user (getenv (if (equal system-type 'windows-nt)
                                 "USERNAME"
                               "USER")))

(defvar config-dir (concat user-emacs-directory "config/")
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
                         ("melpa"        . "https://melpa.org/packages/"))
      package-pinned-packages nil
      package-user-dir (concat data-dir "site-lisp"))

;; package-archive-priorities
;; '(("melpa-stable" . 10)
;;   ("gnu"          .  5)
;;   ("melpa"        .  0)))

(package-initialize)

;;;; Bootstrap `req-package' from Mepla stable

(unless (package-installed-p 'req-package)
  (let ((package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/"))))
      (progn
        (package-refresh-contents)
        (package-install 'req-package))))
(require 'req-package)

;;;; Setup `load-path'

;; Add Homebrew site-lisp direcotry to load-path if present
(when (file-directory-p "/usr/local/share/emacs/site-lisp/")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/"))

;; Add packages provided by Nix to load-path
(let ((default-directory "/run/current-system/sw/share/emacs/site-lisp/"))
  (when (file-directory-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path config-dir)

(req-package exec-path-from-shell
  :pin melpa-stable
  :if (memq window-system '(mac ns))
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

(req-package leuven
  :pin melpa-stable
  :init
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

;;; Convenience

;;;; iBuffer

(bind-key "C-x B" 'ibuffer)

;;; Files

(setq recentf-save-file (concat cache-dir "recentf"))
(setq recentf-max-menu-items 100
      recentf-exclude '("/.git/"))
(recentf-mode 1)

(setq auto-save-list-file-prefix (concat cache-dir "auto-save-list/saves-"))

(setq backup-directory-alist `((".*" . ,(concat cache-dir "backups/"))))

(global-auto-revert-mode 1)

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

;;;; Smartparens

;; https://ebzzry.github.io/emacs-pairs.html

(req-package smartparens
  :require smartparens-config
  :pin melpa-stable
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

;; (setq mu4e-header-skip duplicates t)
;; (setq mu4e-headers-show-threads t)
;; (setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
;;       mu4e-headers-fields '((:date . 20)
;;                          (:flags . 5)
;;                          (:mailing-list . 10)
;;                          (:from-or-to . 25)
;;                          (:subject . nil)))
;; (setq mu4e-view-show-addresses t)
;; (setq message-kill-buffer-on-exit t
;;       mu4e-sent-messages-behavior 'delete)

(req-package mu4e
  ;; Provided by nixpkgs.mu
  :ensure nil
  :init
  (setq user-full-name "Per K. Dahl")
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtpq")
        message-kill-buffer-on-exit t)
  (setq mu4e-maildir "~/mail"
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "true"
        mu4e-attachment-dir "~/Downloads")
  :config
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Gmail"
             :enter-func (lambda () (mu4e-message "Entering Gmail context"))
             :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
             :match-func (lambda (msg)
                           (when msg
                             (string= (mu4e-message-field msg :maildir) "/gmail")))
             :vars '( ( user-mail-address           . "pkdahl@gmail.com" )
                      ( mu4e-sent-folder            . "/gmail/sent" )
                      ( mu4e-drafts-folder          . "/gmail/drafts" )
                      ( mu4e-trash-folder           . "/gmail/trash" )
                      ( mu4e-refile-folder          . "/gmail/all")
                      ( mu4e-sent-messages-behavior . 'delete )))
           ,(make-mu4e-context
             :name "UiO"
             :enter-func (lambda () (mu4e-message "Switch to UiO context"))
             :leave-func (lambda () (mu4e-message "Leaving UiO context"))
             :match-func (lambda (msg)
                           (when msg
                             (string= (mu4e-message-field msg :maildir) "/uio")))
             :vars '( ( user-mail-address  . "perkda@ifi.uio.no" )
                      ( mu4e-sent-folder   . "/uio/sent" )
                      ( mu4e-drafts-folder . "/uio/drafts" )
                      ( mu4e-trash-folder  . "/uio/trash" )
                      ( mu4e-refile-folder . "/uio/received" )))))
  ;; Don't apply trashed flag, just move
  ;; FIX We get code for trash twice in the list
  (add-to-list 'mu4e-marks
               '(trash :char ("d" . "▼")
                       :prompt "dtrash"
                       :dyn-target (lambda (target msg)
                                     (mu4e-get-trash-folder msg))
                       :action (lambda (docid msg target)
                                 (mu4e~proc-move docid
                                   (mu4e~mark-check-target target) "-N")))))

(req-package org-mu4e
  :require (mu4e org-mode))

;;; Multimedia

(setq image-dired-dir (concat cache-dir "image-dired/"))

;;; Programming

;;;; Languages

;;;;; Elm

(req-package elm-mode
  :pin melpa-stable
  :mode "\\.elm\\'")

;;;;; Haskell

(req-package haskell-mode
  :pin melpa-stable
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;;;;; Scheme

(req-package geiser
  :pin melpa-stable
  :config
  (setq geiser-active-implementations '(guile)))

;;;; Tools

;;;;; Magit

(req-package magit
  :pin melpa-stable
  :bind ("C-x g" . magit-status))

;;; Text

;;;; Outlines

;;;;; Org

(unless (package-installed-p 'org-plus-contrib)
  (let ((package-archives '(("org" . "http://orgmode.org/elpa/"))))
    (progn (package-refresh-contents)
           (package-install 'org-plus-contrib))))

(req-package org
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
                          (whitespace-cleanup)))))

  (when (file-exists-p "~/notes/.emacs.el")
    (load-file "~/notes/.emacs.el"))

  (use-package org-mac-link
  :if (string-equal system-type "darwin")
  :bind ("C-c m" . org-mac-grab-link)))

(req-package org-agenda
  :require org
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-start-on-weekday nil
        org-agenda-time-leading-zero t))

(req-package org-capture
  :require org
  :demand t
  :bind
  ("C-c c" . org-capture))

(req-package org-id
  :require org
  :config
  (setq org-id-locations-file (concat cache-dir "org-id-location")))

;;;;; Outshine

;; Enable org-mode-like folding in emacs-lisp-mode

(use-package outshine
  :pin melpa
  :ensure t
  :diminish outline-minor-mode
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :config
  (setq outshine-startup-folded-p t
        outshine-use-speed-commands t))

;;;; Markdown

(req-package markdown-mode
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

(setq custom-file (concat config-dir "custom.el"))

;;; Finalize

(load custom-file 'noerror)
(req-package-finish)
