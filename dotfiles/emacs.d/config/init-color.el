;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :config
  (load-theme 'tango-dark t)
  (load-theme 'doom-molokai t)
  )

(use-package rainbow-delimiters 
  :config
  (progn
    ;; Enable in all programming-related modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package powerline
  :config
  (powerline-center-evil-theme))
(add-hook 'after-init-hook 'powerline-reset)
(use-package powerline-evil)
;;(add-to-list 'default-frame-alist '(background-color . "honeydew2"))

(provide 'init-color)
