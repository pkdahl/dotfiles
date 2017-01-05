;;; markdown-setup.el

(req-package markdown-mode
  :pin melpa-stable
  :mode "\\.md\\'"
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1))))

(provide 'markdown-setup)
