;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :config
  ;; (load-theme 'tango-dark t)
  ;; (load-theme 'doom-molokai t)
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

(use-package doom-modeline
  :after evil doom-themes
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-icon nil)
  ;; (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (set-cursor-color "cyan")
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 4)
  (nyan-animate-nyancat t)
  :hook
  (doom-modeline-mode . nyan-mode))

(use-package rainbow-delimiters 
  :config
  (progn
    ;; Enable in all programming-related modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; (use-package powerline
;;   :config
;;   (powerline-center-evil-theme))
;; (add-hook 'after-init-hook 'powerline-reset)
;; (use-package powerline-evil)
;;(add-to-list 'default-frame-alist '(background-color . "honeydew2"))

(provide 'init-color)
