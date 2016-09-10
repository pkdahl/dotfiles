;;; init-packages.el

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	;; ("melpa" . "https://melpa.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))

;; (add-to-list 'package-archives
;;	     '("org" . "http://orgmode.org/elpa/") t)

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages nil))

(package-initialize)

(provide 'init-packages)
