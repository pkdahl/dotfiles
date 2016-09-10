;;; init-helm.el

(req-package helm
  :pin melpa-stable
  :diminish helm-mode
  :bind
  ("C-c h m"   . helm-mini)
  ("C-c h h"   . helm-org-headlines)
  ("C-x C-m" . helm-M-x)
  :init (helm-mode 1)
  :config
  (add-to-list 'helm-boring-file-regexp-list '"\\.DS_Store" t)
  (add-to-list 'helm-boring-file-regexp-list '"\\.Trashes" t)
  (setq helm-ff-skip-boring-files t))

(provide 'init-helm)
