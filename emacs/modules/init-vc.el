;;; init-vc.el

(req-package magit
  :pin melpa-stable
  :require package
  :bind ("C-x g" . magit-status))

(req-package git-annex
  :pin melpa-stable
  :if (executable-find "git-annex")
  :require dired
  :config
  (setq git-annex-commit nil))

(req-package magit-annex
  :if
  (executable-find "git-annex")
  :require
  (magit
   git-annex))

(req-package git-timemachine
  :pin melpa-stable)

(provide 'init-vc)
