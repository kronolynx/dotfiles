;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :config
  ;; (load-theme 'doom-molokai t)
  (load-theme 'doom-dracula t)
  (with-eval-after-load 'org
    (doom-themes-org-config))
  )

(use-package doom-modeline
  ;; :disabled
  :after (evil doom-themes)
  :custom
  (column-number-mode nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-evil-state-icon t)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-unicode-fallback t)
  (line-number-mode nil)
  (set-cursor-color "cyan")
  ;; (doom-modeline-minor-modes (featurep 'minions))
  (doom-modeline-minor-modes t)
  :hook
  (after-init . doom-modeline-mode)
  )

(use-package minions
  :config (minions-mode 1)
  )
(use-package nyan-mode
  :disabled
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

(use-package emojify
  :commands emojify-mode
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-display-style 'unicode)
  :hook ((after-init . global-emojify-mode)))

;; (use-package powerline
;;   :config
;;   (powerline-center-evil-theme))
;; (add-hook 'after-init-hook 'powerline-reset)
;; (use-package powerline-evil)
;;(add-to-list 'default-frame-alist '(background-color . "honeydew2"))


(provide 'init-color)
