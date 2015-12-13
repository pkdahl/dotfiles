;;; init-javascript.el

(req-package js2-mode
  :pin melpa-stable
  :mode "\\.js\\'"
  :interpreter "node")

(req-package company-tern
  :disabled t
  :pin melpa-stable
  :require (company
	    js2-mode
	    tern)
  :init (add-to-list 'company-backends 'company-tern))

(req-package tern
  :disabled t
  :pin melpa-stable
  :if (executable-find "tern")
  :init (add-hook 'js2-mode-hook 'tern-mode))

(provide 'init-javascript)
