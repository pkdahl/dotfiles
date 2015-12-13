;;; init-bootstrap.el

;; Install `req-package' from Melpa stable if not already on the system
(let ((package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/"))))
  (progn
    (package-initialize)
    (unless (package-installed-p 'req-package)
      (progn
	(package-refresh-contents)
	(package-install 'req-package)))))

(require 'req-package)

(provide 'init-bootstrap)
