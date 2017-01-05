;;; ~/emacs.d/config/package-setup.el

(setq package-user-dir (concat data-dir "site-lisp"))

;; Install `req-package from Melpa stable
(let ((package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/"))))
  (progn
    (package-initialize)
    (unless (package-installed-p 'req-package)
      (progn
        (package-refresh-contents)
        (package-install 'req-package)))))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages nil))

(require 'req-package)

(provide 'package-setup)
