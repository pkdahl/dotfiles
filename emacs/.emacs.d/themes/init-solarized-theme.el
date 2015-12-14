;; init-solarized-theme.el

(req-package solarized-theme
  :pin melpa-stable
  :demand t
  :init
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil
	solarized-use-less-bold t
	;; Do not scale font up
	solarized-height-plus-1 1.0
	solarized-height-plus-2 1.0
	solarized-height-plus-3 1.0
	solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))

(provide 'init-solarized-theme)
