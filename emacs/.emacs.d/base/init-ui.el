;;; init-ui.el

(setq inhibit-startup-message t)

(dolist (mode '(blink-cursor-mode
		menu-bar-mode
		scroll-bar-mode
		tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(column-number-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq default-frame-alist '((width . 166)
			    (height . 50)))
(setq split-width-threshold 100)

(when (member "DejaVu Sans Mono" (font-family-list))
  (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-12"))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12")))

(req-package init-solarized-theme
  :config
  (load-theme 'solarized-light t))

(provide 'init-ui)
