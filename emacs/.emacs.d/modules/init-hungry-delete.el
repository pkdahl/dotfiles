;;; init-hungry-delete.el

(req-package hungry-delete
  :pin "marmalade"
  :require package
  :init (global-hungry-delete-mode))

(provide 'init-hungry-delete)
