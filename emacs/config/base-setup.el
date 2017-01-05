;;; base-setup

;; recentf

(setq recentf-save-file (concat cache-dir "recentf"))
(setq recentf-max-menu-items 100
      recentf-exclude '("/.git/"))
(recentf-mode 1)

(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat cache-dir "bookmarks"))

(setq auto-save-list-file-prefix (concat cache-dir "auto-save-list/saves-"))
(setq backup-directory-alist `((".*" . ,(concat cache-dir "backups/"))))

(setq image-dired-dir (concat cache-dir "image-dired/"))
(setq url-configuration-directory (concat cache-dir "url/"))
(setq tetris-score-file (concat cache-dir "tetris-scores"))

;; EasyPG Assistant

(setq epg-gpg-program "gpg2")
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(provide 'base-setup)
