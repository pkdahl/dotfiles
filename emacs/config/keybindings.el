;;; ~/.emacs.d/config/keybindings.el

;; uses use-package's bind-key
;; req-package is built on top of use-package

(bind-key "C-x B" 'ibuffer)
(bind-key "M-;" 'comment-or-uncomment-region)
;;(bind-key "A-w c" 'whitespace-cleanup)

(provide 'keybindings)
