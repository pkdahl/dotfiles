;; ~/emacs.d/config/geiser-setup.el

(req-package geiser
  :pin melpa-stable
  :config
  (setq geiser-active-implementations '(guile)))

(provide 'geiser-setup)
