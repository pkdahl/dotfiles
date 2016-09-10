;;; init-python.el

;; TODO
;; Install package ein for Jupyter inside Emacs

(req-package elpy
  :pin melpa-stable
  :require flycheck
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Installed from Melpa?
(req-package python-django
  :require python)

(provide 'init-python)
