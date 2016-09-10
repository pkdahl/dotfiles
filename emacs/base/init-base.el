;;; init-base.el

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Dired

(when (executable-find "gls")
  (setq ls-lisp-use-insert-directory-program (executable-find "gls")))

;; EasyPG Assistant

(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epg-gpg-program "gpg1")

;; recentf

(setq recentf-save-file (concat data-dir "recentf"))
(setq recentf-max-menu-items 20
      recentf-exclude '("/.git/"))
(recentf-mode 1)

(setq bookmark-save-flag 1)

(setq vc-make-backup-files t)


;; Locations

(setq auto-save-list-file-prefix (concat cache-dir "auto-save-list/saves-"))
(setq backup-directory-alist `((".*" . ,(concat cache-dir "backups/"))))
(setq bookmark-default-file (concat data-dir "bookmarks"))
(setq image-dired-dir (concat cache-dir "image-dired/"))
(setq tetris-score-file (concat data-dir "tetris-scores"))
(setq url-configuration-directory (concat cache-dir "url/"))

(provide 'init-base)
