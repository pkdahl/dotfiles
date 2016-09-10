;;; init-swiper.el

(req-package swiper
  :pin "melpa-stable"
  :bind (("C-s" . swiper)
	 ("C-r" . swiper))
  :init (ivy-mode 1))

(provide 'init-swiper)
