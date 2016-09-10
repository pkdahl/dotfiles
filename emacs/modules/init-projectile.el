;;; init-projectile.el

(req-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :init
  (setq projectile-cache-file
	      (expand-file-name "projectile.cache" cache-dir))
  (setq projectile-known-projects-file
	(expand-file-name "projectile-bookmarks.eld" data-dir))
  (projectile-global-mode)
  (projectile-load-known-projects))

(req-package helm-projectile
  :require
  (helm
   projectile)
  :init
  (setq projectile-completion-system 'helm))

(provide 'init-projectile)
