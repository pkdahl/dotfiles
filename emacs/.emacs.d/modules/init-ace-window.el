;;; init-ace-window.el

(req-package ace-window
  :pin melpa-stable
  :bind
  ("H-w" . ace-window)
  :config
  (setq aw-dispatch-always nil)
  (setq aw-keys '(?j ?f ?h ?g ?k ?d ?l ?s ?a)))

(provide 'init-ace-window)
