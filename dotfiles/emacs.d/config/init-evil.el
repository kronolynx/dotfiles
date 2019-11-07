;;----------------------------------------------------------------------------
;; evil
;;----------------------------------------------------------------------------
(use-package evil
  :diminish undo-tree-mode
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init ;; tweak evil's configuration before loading it
  (setq evil-ex-complete-emacs-commands nil
        evil-search-module 'evil-search 
        evil-shift-round nil 
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-u-scroll t
        evil-want-fine-undo t
        evil-want-integration t
        evil-want-keybinding nil)
  :config ;; tweak evil after loading it
  (evil-mode 1)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :commands (evil-embrace-enable-evil-surround-integration
             evil-embrace-disable-evil-surround-integration))

;; Press “%” to jump between matched tags
(use-package evil-matchit
  :config
  (global-evil-matchit-mode))

(use-package evil-nerd-commenter)

(use-package evil-lion
  :config
  (evil-lion-mode))

;; gx operator, like vim-exchange
(use-package evil-exchange
  :disabled
  :bind (:map evil-normal-state-map
              ("gx" . evil-exchange)
              ("gX" . evil-exchange-cancel)))

;; * operator in vusual mode
(use-package evil-visualstar
  :bind (:map evil-visual-state-map
              ("*" . evil-visualstar/begin-search-forward)
              ("#" . evil-visualstar/begin-search-backward)))

;; C-+ C-- to increase/decrease number like Vim's C-a C-x
(use-package evil-numbers
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)))

(use-package evil-magit
  :requires magit
  :after (magit)
  :init
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  )

;; vim like number line
(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t)

(defun noct:relative ()
  (setq-local display-line-numbers 'visual))

(defun noct:absolute ()
  (setq-local display-line-numbers t))
(add-hook 'evil-insert-state-entry-hook #'noct:absolute)
(add-hook 'evil-insert-state-exit-hook #'noct:relative)


(provide 'init-evil)
