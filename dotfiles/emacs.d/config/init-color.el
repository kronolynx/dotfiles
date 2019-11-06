;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'tango-dark t)
  (load-theme 'doom-molokai t))

(use-package rainbow-delimiters :ensure t
  :config
  (progn
    ;; Enable in all programming-related modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'init-color)
