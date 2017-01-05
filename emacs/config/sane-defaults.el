;;; sane-default.el

(setq inhibit-startup-message t)

(dolist (mode '(blink-cursor-mode
		scroll-bar-mode
		tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; if not on os x turn of menu-bar-mode too
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))

(column-number-mode 1)
(show-paren-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq sentence-end-double-space nil)

(setq-default fill-column 79
	      truncate-lines t
	      indent-tabs-mode nil      ; spaces instead of tabs by default
	      split-width-threshold 100)

(set-language-environment "UTF-8")

(global-auto-revert-mode 1)

(provide 'sane-defaults)
