;;; ui-setup.el

(req-package leuven
  :pin melpa-stable
  :init
  (load-theme 'leuven t))

(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro"))

(setq default-frame-alist '((width . 120)
			    (height . 50)))

(provide 'ui-setup)
