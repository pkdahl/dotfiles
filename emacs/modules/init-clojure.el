;;; init-clojure.el

(req-package cider
  :require clojure)

(req-package cljoure-mode
  :init
  (add-hook 'cljure-mode-hook 'enable-paredit-mode))

(provide 'init-clojure)

