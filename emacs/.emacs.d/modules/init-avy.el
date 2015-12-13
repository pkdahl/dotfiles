;;; init-avy.el

(req-package avy
  :pin melpa-stable
  :config
  (global-set-key (kbd "M-g g") #'avy-goto-line)
  (eval-after-load 'hydra
    '(defhydra hydra-avy (global-map "M-g" :color blue :columns 4)
       "avy-goto"
       ("c" avy-goto-char "char")
       ("C" avy-goto-char-2 "char-2")
       ("w" avy-goto-word-1 "word")
       ("s" avy-goto-subword-1 "subword"))))

(provide 'init-avy)
