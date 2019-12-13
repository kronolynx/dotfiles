;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ;if nil, italics is universally disabled
  :config
  ;; (load-theme 'doom-molokai t)
  (load-theme 'doom-dracula t)
  (with-eval-after-load 'org
    (doom-themes-org-config))
  )

(use-package doom-modeline
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
  (doom-modeline-minor-modes t)
  :config
  (progn
    (defun enable-doom-modeline-icons (_frame)
      (setq doom-modeline-icon t))
    (add-hook 'after-make-frame-functions
	      #'enable-doom-modeline-icons)
    )
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

;; matching parentheses
;;(use-package faces
;;  :ensure nil
;;  :custom (show-paren-delay 0)
;;  :config
;;  (set-face-background 'show-paren-match "#262b36")
;;  (set-face-bold 'show-paren-match t)
;;  (set-face-foreground 'show-paren-match "#ffffff"))

(use-package emojify
  :commands emojify-mode
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-display-style 'unicode)
  :hook ((after-init . global-emojify-mode)))



(provide 'base-theme)
