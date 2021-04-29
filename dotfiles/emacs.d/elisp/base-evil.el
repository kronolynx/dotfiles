;;----------------------------------------------------------------------------
;; evil
;;----------------------------------------------------------------------------
(use-package evil
  :diminish undo-tree-mode
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :custom
  (evil-mode-line-format nil)
  (evil-normal-state-cursor '(box "orchid"))
  (evil-normal-state-cursor '(box "dark gray"))
  (evil-motion-state-cursor '(box "YellowGreen"))
  (evil-insert-state-cursor '(bar "White"))
  (evil-emacs-state-cursor '(bar "Red"))
  (evil-visual-state-cursor '(box "#F86155"))
  (global-subword-mode 1)
  :init ;; tweak evil's configuration before loading it
  (setq evil-ex-complete-emacs-commands nil
	evil-ex-search-persistent-highlight nil
	evil-search-module 'evil-search
	evil-shift-round nil
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t
	evil-want-fine-undo t
	evil-want-integration t
	evil-want-keybinding nil)
  :config ;; tweak evil after loading it
  (general-def 'insert
    "C-o" #'evil-execute-in-normal-state
    "C-r" #'evil-paste-from-register
    "C-w" #'evil-delete-backward-word
    ;;"jj" #'evil-normal-state
    )
  (evil-mode 1)
  ;;(define-key evil-insert-state-map "jj" 'evil-normal-state)
  )

(use-package evil-escape
  :after evil
  :config
  (progn
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "jj")
    (setq-default evil-escape-delay 0.2)
    (push 'visual evil-escape-excluded-states)
    )
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :after evil
  :commands (evil-embrace-enable-evil-surround-integration
             evil-embrace-disable-evil-surround-integration))

;; Press “%” to jump between matched tags
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-nerd-commenter
  :after evil
  )

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; gx operator, like vim-exchange
(use-package evil-exchange
  :after evil
  :bind (:map evil-normal-state-map
              ("gx" . evil-exchange)
              ("gX" . evil-exchange-cancel)))

;; * operator in vusual mode
(use-package evil-visualstar
  :after evil
  :bind (:map evil-visual-state-map
              ("*" . evil-visualstar/begin-search-forward)
              ("#" . evil-visualstar/begin-search-backward)))

;; C-+ C-- to increase/decrease number like Vim's C-a C-x
(use-package evil-numbers
  :after evil
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)))

(use-package evil-magit
  :requires magit
  :after (evil magit)
  :init
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  )

;; vim like number line
(setq-default display-line-numbers 'visual
              ;; this is the default
              display-line-numbers-current-absolute t)

(use-package evil-indent-textobject
  :after evil
  )

(use-package evil-org
  :after evil
  :config
  (evil-org-set-key-theme
   '(textobjects insert navigation additional shift todo heading))
  (add-hook 'org-mode-hook (lambda () (evil-org-mode))))

(use-package evil-terminal-cursor-changer
  :after evil
  :init
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    )
  )

(provide 'base-evil)
