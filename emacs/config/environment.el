;;; ~/.emacs.d/config/environment.el

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt)
       "USERNAME"
     "USER")))

(defvar config-dir (concat user-emacs-directory "config/")
  "Directory for emacs configuration files.")

;; (defvar private-config-dir ".config/emacs/"
;;   "Directory for private emacs config files.")

;; (unless (file-directory-p private-config-dir)
;;   (make-directory private-config-dir))

(defvar cache-dir "~/.cache/emacs/"
  "Directory for temporary emacs files.")

(unless (file-directory-p cache-dir)
  (make-directory cache-dir t))

(defvar data-dir "~/.local/share/emacs/"
  "Directory for auxiliary emacs files.")

(unless (file-directory-p data-dir)
  (make-directory data-dir t))

;; Setup load-path

;; Add Nix' site-lisp directory
(let ((default-directory "/run/current-system/sw/share/emacs/site-lisp/"))
  (when (file-directory-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path config-dir)
