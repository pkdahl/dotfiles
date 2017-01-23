;; (setq mu4e-header-skip duplicates t)
;; (setq mu4e-headers-show-threads t)
;; (setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
;;       mu4e-headers-fields '((:date . 20)
;;                          (:flags . 5)
;;                          (:mailing-list . 10)
;;                          (:from-or-to . 25)
;;                          (:subject . nil)))
;; (setq mu4e-view-show-addresses t)
;; (setq message-kill-buffer-on-exit t
;;       mu4e-sent-messages-behavior 'delete)

(req-package mu4e
  :ensure nil  ; provided by nixpkgs.mu
  :init
  (setq user-full-name "Per K. Dahl")
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtpq")
        message-kill-buffer-on-exit t)
  (setq mu4e-maildir "~/mail"
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "true"
        mu4e-attachment-dir "~/Downloads")
  :config
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Gmail"
             :enter-func (lambda () (mu4e-message "Entering Gmail context"))
             :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
             :match-func (lambda (msg)
                           (when msg
                             (string= (mu4e-message-field msg :maildir) "/gmail")))
             :vars '( ( user-mail-address           . "pkdahl@gmail.com" )
                      ( mu4e-sent-folder            . "/gmail/sent" )
                      ( mu4e-drafts-folder          . "/gmail/drafts" )
                      ( mu4e-trash-folder           . "/gmail/trash" )
                      ( mu4e-refile-folder          . "/gmail/all")
                      ( mu4e-sent-messages-behavior . 'delete )))
           ,(make-mu4e-context
             :name "UiO"
             :enter-func (lambda () (mu4e-message "Switch to UiO context"))
             :leave-func (lambda () (mu4e-message "Leaving UiO context"))
             :match-func (lambda (msg)
                           (when msg
                             (string= (mu4e-message-field msg :maildir) "/uio")))
             :vars '( ( user-mail-address  . "perkda@ifi.uio.no" )
                      ( mu4e-sent-folder   . "/uio/sent" )
                      ( mu4e-drafts-folder . "/uio/drafts" )
                      ( mu4e-trash-folder  . "/uio/trash" )
                      ( mu4e-refile-folder . "/uio/received" )))))
  ;; Don't apply trashed flag, just move
  ;; FIX We get code for trash twice in the list
  (add-to-list 'mu4e-marks
               '(trash :char ("d" . "▼")
                       :prompt "dtrash"
                       :dyn-target (lambda (target msg)
                                     (mu4e-get-trash-folder msg))
                       :action (lambda (docid msg target)
                                 (mu4e~proc-move docid
                                   (mu4e~mark-check-target target) "-N")))))

(req-package org-mu4e
  :require (mu4e org-mode))

(provide 'mail-setup)
