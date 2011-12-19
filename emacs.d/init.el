;;;;
;;;; .emacs.d/init.el
;;;;

;; Do not display splash screen on startup
(setq inhibit-splash-screen t)

;; Show column numbers
(column-number-mode t)

;; Set Dropbox directory
(if (eq system-type 'windows-nt)
    (setq dropbox-directory "c:/Users/dahl/Dropbox")
  (setq dropbox-directory "~/Dropbox"))

;; Add cygwin to path
(if (file-directory-p "c:/bin/cygwin/bin")
    (add-to-list 'exec-path "c:/bin/cygwin/bin"))

;;; Backup

;; Put backup files in one directory
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/backups/")))

;; Copy to create backup files
(setq backup-by-copying t)

;; Make backup versions
(setq version-control t)

;; Backup versions to keep (default value "2")
(setq kept-old-versions 2)
(setq kept-new-versions 5)

;; Delete backup files silently
(setq delete-old-versions t)

;;--

;; File types by the file suffix
(setq auto-mode-alist
      (append '(("\\.mss$" . scribe-mode))
	      '(("\\.bib$" . bibtex-mode))
	      '(("\\.tex$" . latex-mode))
	      '(("\\.obj$" . lisp-mode))
	      '(("\\.st$"  . smalltalk-mode))
	      '(("\\.Z$"   . uncompress-while-visiting))
	      '(("\\.cs$"  . indented-text-mode))
	      '(("\\.C$"   . c++-mode))
	      '(("\\.cc$"  . c++-mode))
	      '(("\\.icc$" . c++-mode))
	      '(("\\.c$"   . c-mode))
	      '(("\\.y$"   . c-mode))
	      '(("\\.h$"   . c++-mode))
              '(("\\.org"  . org-mode))
              '(("\\.org_archive" . org-mode))
              '(("\\.rb"   . ruby-mode))
              '(("\\.erb"  . ruby-mode))
	      auto-mode-alist))

;;--

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Mode specific indent distances
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq sh-basic-offset 2)

;; Set timestamps to appear in English
(setq system-time-locale "C")

;; Make sure we have UTF-8 all the time
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; Org mode

(require 'org-install)

;; Standard key bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchchb)
(define-key global-map "\C-cc" 'org-capture)

;; Cleaner outline view
(setq org-startup-indented t)

;; Put time log into drawer
(setq org-log-into-drawer t)

;; Log time when DONE
(setq org-log-done t)

;; Want to create IDs manually
(setq org-link-to-org-use-id 'use-existing)

;; Keep stored links after insertion
(setq org-keep-stored-link-after-insertion t)

;; Follow links with RET
(setq org-return-follows-link t)

;; Set org directories
(setq org-directory (concat dropbox-directory "/org"))
(setq org-mobile-directory (concat dropbox-directory "/MobileOrg"))
(message "Org directory is: %s" org-directory)
(message "MobileOrg directory is: %s" org-mobile-directory)

;; Refile file
(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "/refile.org"))

;; Refile targets
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path t)

;; New note at the beginning of an entry
(setq org-reverse-note-order t)

;; Agenda files
(setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
;;(setq org-agenda-files (list (concat org-directory "/mcg.org")
;;                             (concat org-directory "/personal.org")
;;                             (concat org-directory "/log.org")))

;; Agenda settings
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-list-sublevels t)       ; Check sublevels
(setq org-agenda-tags-match-list-sublevels t) ; Match tags in sublevels

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/@)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
        (sequence "OPEN(o)" "|" "WON (w@)" "LOST(l@)")))

;; TODO faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("STARTED" . (:foreground "blue" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("CANCELLED" . (:foreground "forest green" :weight bold))
        ("DONE" .(:foreground "forest green" :weight bold))
        ("OPEN" . (:foreground "red" :weight bold))
        ("WON" . (:foreground "forest green" :weight bold))
        ("LOST" . (:foreground "forest green" :weight bold))))

;; Allow changing TODO states with S-left/-right without the normal processing
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Capture templates
(setq org-capture-templates
      '(("t" "todo" entry (file (concat org-directory "/refile.org"))
         "* TODO %?\nCREATED: %U")
        ("n" "note" entry (file (concat org-directory "/refile.org"))
         "* %?\nCREATED: %U")
        ("o" "todo" entry (file (concat org-directory "/refile.org"))
         "* OPEN %?\nCREATED: %U")
        ("l" "log" entry (file+datetree (concat org-directory "/log.org"))
         "* %U %?")
        ("r" "reference" entry (file (concat org-directory "/reference.org"))
         "* %? %^g\n")
        ("p" "reference with attachment" entry (file (concat org-directory "/reference.org"))
         "* %? %^g\n:PROPERTIES:\n:ID: \n:ATTACH_DIR: \n:Date: \n:Filed: %<%Y-%m-%d %H:%M>\n:END:\n")))

;; Archiving

;; Do not change TODO state to DONE when archiving
(setq org-archive-mark-done nil)

;;--

(message "* -- [ .emacs.d/init.el loaded ] --")
