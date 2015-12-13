;;; org-config.el

(with-eval-after-load 'org
  (setq org-directory "~/Documents/Org/")
  (setq org-default-notes-file
	(concat org-directory "captures.org"))
  (setq org-refile-targets
	'((nil :maxlevel . 5)
	  (org-agenda-files :maxlevel . 5)))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n!)" "WAIT(w@/!)" "HOLD(h@/!)" "|"
		    "DONE(d!)" "CXLD(c@)")))
  (setq org-todo-keyword-faces
	'(("NEXT" :foreground "blue" :weight bold)
	  ("WAIT" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "orange" :weight bold))))

(eval-after-load 'org-agenda
  '(progn
     (setq org-tags-exclude-from-inheritance '("prj"))
     (setq org-stuck-projects '("prj" ("NEXT" "WAIT") nil ""))
     (setq org-agenda-custom-commands
	  '(("d" "Day agenda"
	     ((agenda "Todays agenda"
		      ((org-agenda-span 'day)
		       ;; exclude tasks waiting or on hold unless they have
		       ;; a deadline.
		       (org-agenda-skip-function
			'(unless (org-agenda-skip-entry-if 'deadline)
			   (or
			    (org-agenda-skip-entry-if 'todo '("WAIT"))
			    (org-agenda-skip-entry-if 'todo '("HOLD"))))))
		      (todo "NEXT"
			    ((org-agenda-overriding-header "Next actions (unscheduled)")
			     (org-agenda-skip-function
			      '(org-agenda-skip-entry-if 'scheduled 'deadline)))))))
	    ("i" "Inbox" tags "inbox"
	     ((org-agenda-overriding-header "Inbox")))
	    ("n" "Next actions" todo "NEXT"
	     ((org-agenda-overriding-header "Next actions")
	      (org-agenda-todo-ignore-scheduled 'future)))
	    ("w" "Waiting for" todo "WAIT"
	     ((org-agenda-overriding-header "Waiting for...")))
	    ("p" "Projects" tags "prj"
	     ((org-agenda-overriding-header "Projects")))
	    ("r" "Read/watch" tags "read"
	     ((org-agenda-overriding-header "Reading list")
	      (org-agenda-files '("~/prj/org/read-watch.org"))))
	    ("h" "Habits" tags "STYLE=\"habit\""
	     ((org-habit-show-all-today t)
	      (org-agenda-todo-ignore-with-date nil)
	      (org-agenda-todo-ignore-scheduled nil)
	      (org-agenda-todo-ignore-deadlines nil)))))))

(eval-after-load 'org-capture
  `(setq org-capture-templates
	 `(("t" "Todo" entry (file org-default-notes-file)
	    "* TODO %?\n:PROPERTIES:\n:created_at: %U\n:END:\n\n"
	    :empty-lines 1)
	   ("n" "Note" entry (file org-default-notes-file)
	    "* %?\n:PROPERTIES:\n:created_at: %U\n:END:\n\n"
	    :empty-lines 1)
	   ("j" "Journal entry" entry (file+datetree ,(concat org-directory "journal/journal.org"))
	    "* %?\n%U\n\n%i\nReference: %a"
	    :empty-lines 1)
	   ("w" "Weblog entry" entry (file (concat org-directory "weblog.org"))
	    "* %?\n:PROPERTIES:\n:captured_at: %U\n:source_link:\n:END:\n"))))

(eval-after-load 'org-index
  '(setq org-index-id "30BAA45A-64AD-4CCA-A25B-F085BE9AC261"))

(eval-after-load 'org-mobile
  `(setq org-mobile-directory "~/Dropbox/MobileOrg"
	 org-mobile-files nil
	 org-mobile-inbox-for-pull ,(concat org-directory
					    "tasks/from-mobile.org")))

(when (file-exists-p "~/Documents/Org/.emacs.el")
  (load-file "~/Documents/Org/.emacs.el"))

;; Remove empty drawer in buffer
;; http://orgmode.org/worg/org-hacks.html

(defun pxd--org-remove-empty-logbook-drawers-in-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":LOGBOOK:" nil t)
      (save-excursion
	(org-remove-empty-drawer-at "LOGBOOK" (match-beginning 0))))))

(provide 'org-config)
