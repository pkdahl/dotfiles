;;; init-ledger.el

(req-package ledger-mode
  :config 
  (setq ledger-highlight-xact-under-point nil
	ledger-use-iso-dates t
	ledger-post-auto-adjust-amounts t
	ledger-init-file-name "~/.ledgerrc"))

(provide 'init-ledger)
