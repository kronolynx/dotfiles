;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:

;;----------------------------------------------------------------------------
;; packages
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-editor)
(require 'init-package)
(require 'init-general)
(require 'init-gui)
(require 'init-evil)
(require 'init-color)
(require 'init-prog-lang)

;; copy to clipboard for -nw
(use-package xclip
  :init
  (xclip-mode 1))


(use-package ivy
  :delight
  :demand t
  :init
  (ivy-mode 1)                          ; enable ivy globally at startup
  :bind (:map ivy-minibuffer-map        ; bind in the ivy buffer
              ("RET" . ivy-alt-done)
              ("s-<"   . ivy-avy)
              ("s->"   . ivy-dispatching-done)
              ("s-+"   . ivy-call)
              ("s-!"   . ivy-immediate-done)
              ("s-["   . ivy-previous-history-element)
              ("s-]"   . ivy-next-history-element))
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
  ;; TODO testing out the fuzzy search
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil))


(use-package counsel 
  :general
  (general-define-key
   :keymaps 'normal
   "SPC f f" 'counsel-find-file
   "SPC h f" 'counsel-describe-function
   "SPC u"   'counsel-unicode-char
   "SPC p f" '(counsel-git :which-key "find file in git dir")
   "SPC p s" 'counsel-rg
   "SPC SPC" 'counsel-M-x))

(use-package swiper
  :general
  (general-define-key
   :keymaps 'normal
   "SPC s" 'swiper))


;; display help for key usage
(use-package which-key 
  :diminish which-key-mode
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode 1))

(use-package avy 
  :commands (avy-goto-word-1))

(use-package flycheck 
  :delight
  :config
  (global-flycheck-mode))

(use-package ranger
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
  (setq ranger-cleanup-eagerly t)
  )
(use-package git-gutter 
  :delight
  :config
  (global-git-gutter-mode +1))


;;----------------------------------------------------------------------------
;; misc
;;----------------------------------------------------------------------------
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (setq aggressive-indent-sit-for-time 0.5))

(use-package magit
  :general
  (general-define-key
   :keymaps 'normal
   "SPC g s" 'magit-status))


;; Highlighting TODO keywords
(use-package hl-todo
  :config (global-hl-todo-mode))

;; rss feed
(use-package elfeed-goodies)
(use-package elfeed-web)
(use-package elfeed
  :config
  (setq elfeed-db-directory (expand-file-name "feeds" user-emacs-directory)))

;; Automatically refreshes the buffer for changes outside of Emacs
(use-package autorevert
  :delight auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))

;; Show matching parentheses
(use-package paren 
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet                  ; Snippets
  :defer t
  :diminish (yas-minor-mode . " Ⓨ"))

(use-package company                    ; Graphical (auto-)completion
  ;; :diminish company-mode
  :delight
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t)
  )

                                        ; projectile
(use-package projectile
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy
          projectile-switch-project-action 'projectile-dired
          projectile-remember-window-configs t
          projectile-use-git-grep 1)))

(use-package paredit
  :delight
  :config (progn (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
                 (add-hook 'lua-mode-hook 'paredit-mode)))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package all-the-icons) ;; don't forget to M-x all-the-icons-install-fonts

(use-package neotree
  :after evil
  :init
  (evil-set-initial-state 'neotree-mode 'normal)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (defun neo-buffer--insert-header ()
      (let ((start (point)))
        (set-text-properties start (point) '(face neo-header-face)))
      (neo-buffer--newline-and-begin))
    ))
