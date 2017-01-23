;; ~/emacs.d/config/paredit-setup.el

(req-package paredit
  :pin melpa-stable
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interactive-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode)))

(provide 'paredit-setup)
