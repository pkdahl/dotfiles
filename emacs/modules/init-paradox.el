;;; init-paradox.el

(req-package paradox
  :pin "melpa-stable"
  :require package
  :init (setq paradox-github-token t))

(provide 'init-paradox)
