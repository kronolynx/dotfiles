;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:

;;----------------------------------------------------------------------------
;; packages
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/lang" user-emacs-directory))

(require 'init-editor)
(require 'init-package)
(require 'init-general)
(require 'init-gui)
(require 'init-evil)
(require 'init-color)
(require 'init-lang)

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
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
  )


(use-package counsel)
(use-package swiper)
(use-package smex
  :config
  (smex-initialize))


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
  :config
  (setq ranger-cleanup-eagerly t)
  )

(use-package git-gutter
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
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

(use-package google-this)
(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "de"))

(use-package magit)


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


;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet                  ; Snippets
  :defer t
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package company                    ; Graphical (auto-)completion
  :diminish company-mode
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  :init (global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))
  )

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy
          projectile-switch-project-action 'projectile-dired
          projectile-remember-window-configs t
          projectile-use-git-grep 1)))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (setq sp-show-pair-from-inside nil)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  )

;; Show matching parentheses
(use-package paren
  :disabled
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))

(use-package all-the-icons) ;; don't forget to M-x all-the-icons-install-fonts

(use-package neotree
  :after evil projectile
  :custom
  (neo-smart-open t)
  (neo-theme 'icons)
  (neo-window-fixed-size nil)
  (neo-reset-size-on-open t)
  (projectile-switch-project-action 'neotree-projectile-action)
  :init
  (evil-set-initial-state 'neotree-mode 'normal)
  :bind
  ("<f8>" . neotree-toggle)
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  )

(use-package dashboard
  :disabled
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 4)
  (dashboard-items '((recents . 15)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

(use-package docker-tramp)
