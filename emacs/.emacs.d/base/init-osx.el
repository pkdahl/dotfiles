;;; init-osx.el

;; Add Homebrew site-lisp to load-path

(when (file-directory-p "/usr/local/share/emacs/site-lisp/")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/"))

;; - Need option key with Norwegian keyboard
;; - Use command key as =meta=
;; - fn-key is =hyper=
;; - Right command key is =super=
;; - Right option key is =alt=

(setq ns-function-modifier 'hyper
      ns-option-modifier 'nil          ; ns-alternate-modifier
      ns-command-modifier 'meta        ; mac-command-modifier
      ns-right-command-modifier 'super ; mac-right-command-modifier
      ns-right-option-modifier 'alt)

(menu-bar-mode 1)

(req-package exec-path-from-shell
  :pin melpa-stable
  :if (memq window-system '(mac ns))
  :init (exec-path-from-shell-initialize))

(provide 'init-osx)
