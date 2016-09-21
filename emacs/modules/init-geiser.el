(req-package geiser
  :pin melpa-stable
  :require company-mode
  :init
  (setq geiser-active-implementations '(racket))
  (show-paren-mode 1)
)
