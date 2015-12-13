;; init-solarized-theme.el

(req-package solarized-theme
  :pin melpa-stable
  :demand t
  :init
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil
	solarized-use-less-bold t)
  (load-theme 'solarized-light t))

(provide 'init-solarized-theme)
