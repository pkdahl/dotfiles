;;; org-setup.el

(req-package org
  :bind (("C-c l"   . org-store-link)
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
    (load-file "~/notes/.emacs.el")))

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

(provide 'org-setup)
