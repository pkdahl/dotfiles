;;; ~/emacs.d/config/haskell-setup.el

(req-package haskell-mode
  :pin melpa-stable
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(provide 'haskell-setup)
