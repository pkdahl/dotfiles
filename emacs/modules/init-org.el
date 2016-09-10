;;; init-org.el

(setq org-lisp-dir "site-lisp/org-8.3.1/lisp/"
      org-contrib-lisp-dir "site-lisp/org-8.3.1/contrib/lisp/")

(req-package org
  :load-path "site-lisp/org-8.3.1/lisp/"
  :demand t
  :bind
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link)
  ("C-c b"   . org-iswitchb)
  ("H-u"     . outline-up-heading)
  ("H-n"     . outline-next-visible-heading)
  ("H-p"     . outline-previous-visible-heading)
  ("H-f"     . org-forward-heading-same-level)
  ("H-b"     . org-backward-heading-same-level)
  :config
  (eval-after-load 'hydra
      ;; TODO set key for this hydra
    '(defhydra hydra-org (:color red :columns 3)
       "Org Mode Movements"
       ("n" outline-next-visible-heading "next heading")
       ("p" outline-previous-visible-heading "prev heading")
       ("N" org-forward-heading-same-level "next heading at same level")
       ("P" org-backward-heading-same-level "prev heading at same level")
       ("u" outline-up-heading "up heading")
       ("g" org-goto "goto" :exit t)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook
			(lambda ()
			  (whitespace-cleanup)))))

  (setq org-email-link-description-format "Email %c: %.40s"
	org-image-actual-width '(450)
	org-use-speed-commands t
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-speed-commands-default t
	org-startup-folded t
	org-startup-indented t
	org-indent-mode-turns-on-hiding-stars t
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-use-outline-path 'file
	org-enforce-todo-dependencies t
	org-treat-S-cursor-todo-selection-as-state-change nil
	org-log-done 'time
	org-log-into-drawer t
	org-log-redeadline 'note
	org-log-reschedule 'note
	org-clock-into-drawer t
	org-clock-out-remove-zero-time-clocks t
	org-outline-path-complete-in-steps nil
	org-modules '(org-id
		      org-habit)))

(req-package org-agenda
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-inhibit-startup nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
	org-agenda-skip-deadline-prewarning-if-scheduled t
	org-agenda-start-on-weekday nil
	org-agenda-time-leading-zero t
	org-agenda-window-setup 'current-window))

(req-package org-attach
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org
  :config
  (setq org-attach-store-link-p 'attached))

(req-package org-capture
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org
  :demand t
  :bind
  ("C-c c" . org-capture))

(req-package org-id
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org
  :config
  (setq org-id-link-to-org-use-id t
	org-id-locations-file (concat data-dir "org-id-locations")))

(req-package org-mobile
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org)

(req-package ox-publish
  :load-path "site-lisp/org-8.3.1/lisp/"
  :require org
  :config
  (setq org-publish-timestamp-directory (concat cache-dir "org-timestamps/")))

;; Org contrib/ pacakges

(req-package org-mac-link
  :load-path "site-lisp/org-8.3.1/contrib/lisp/"
  :require org
  :bind ("H-l" . org-mac-grab-link))

(req-package org-index
  :load-path "site-lisp/org-8.3.1/contrib/lisp/"
  :require org
  :bind
  ("C-c i" . org-index))

(req-package org-notmuch
  :load-path "site-lisp/org-8.3.1/contrib/lisp/"
  :require (org notmuch)
  :if (executable-find "notmuch")
  :demand t)

(provide 'init-org)
