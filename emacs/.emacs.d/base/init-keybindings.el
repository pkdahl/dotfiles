;;; init-keybindings.el

(bind-key "C-x B" 'ibuffer)
(bind-key "C-," 'comment-or-uncomment-region)
(bind-key "A-w c" 'whitespace-cleanup)

(provide 'init-keybindings)
