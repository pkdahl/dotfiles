;;; init-hydra.el

(req-package hydra
  :init
  (global-set-key
   (kbd "C-z")
   (defhydra hydra-vi (:pre (set-cursor-color "#268bd2") ; blue
			    :post (set-cursor-color "#839496") ; s-base00
			    :color amaranth)
     "vi"
     ("l" forward-char)
     ("h" backward-char)
     ("j" next-line)
     ("k" previous-line)
     ("m" set-mark-command "mark")
     ("a" move-beginning-of-line "beg")
     ("e" move-end-of-line "end")
     ("d" delete-region "del" :color blue)
     ("y" kill-ring-save "yank" :color blue)
     ("x" delete-char "fwd del")
     ("u" undo "undo")
     ("q" nil "quit")))

  (global-set-key
   (kbd "<f11>")
   (defhydra hydra-toggle (:color blue)
     "toggle"
     ("a" auto-fill-mode "auto-fill")
     ("f" fundamental-mode "fundamental-mode")
     ("o" org-mode "org-mode")
     ("i" toggle-input-method "input-method"
      ("q" nil "cancel")))))


;; (defhydra hydra-org (:color red :columns 3)
;;   "Org Mode Movements"
;;   ("n" outline-next-visible-heading "next heading")
;;   ("p" outline-previous-visible-heading "prev heading")
;;   ("N" org-forward-heading-same-level "next heading at same level")
;;   ("P" org-backward-heading-same-level "prev heading at same level")
;;   ("u" outline-up-heading "up heading")
;;   ("g" org-goto "goto" :exit t))

(provide 'init-hydra)
