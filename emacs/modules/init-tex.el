;;; init-tex.el

(req-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'luatex))

(provide 'init-tex)
