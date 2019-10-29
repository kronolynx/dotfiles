;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:

(setq delete-old-versions -1; delete excess backup versions silently
      version-control t; use version control
      vc-make-backup-files t; make backups file even when in version controlled dir
      backup-directory-alist `(("." . "~/.emacs.d/backups")) ; which directory to put backups
      vc-follow-symlinks t       ; don't ask for confirmation when opening symlinked file
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ;transform backups file name
      ring-bell-function 'ignore ; silent bell when you make a mistake
      sentence-end-double-space nil ; sentence SHOULD end with only a point.
      custom-safe-themes t
      scroll-preserve-screen-position t
      )

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)
(tooltip-mode    -1)
(menu-bar-mode   -1)


(defalias 'yes-or-no-p 'y-or-n-p)


(let ((normal-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook #'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-hook 'focus-out-hook #'garbage-collect)

;; save buffers on focus lost
(defun save-all ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)

(add-hook
 'after-init-hook
 (defun my/set-faces ()
   (custom-set-faces
    '(default ((t (:height 130 :family "SauceCodePro Nerd Font" :weight normal)))))
   ))
;;----------------------------------------------------------------------------
;; Load package
;;----------------------------------------------------------------------------
(eval-and-compile
  (require 'package))
(setq package-archives '(
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         )
      )
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;----------------------------------------------------------------------------
;; packages
;;----------------------------------------------------------------------------

;; copy to clipboard for -nw
(use-package xclip :ensure t
  :init
  (xclip-mode 1))


(use-package general :ensure t
  :config
  (general-define-key "C-;" 'avy-goto-word-1)
  (general-define-key "C-s" 'save-all)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; simple command
   "/"   'counsel-ag
   "TAB" '(switch-to-other-buffer :which-key "prev buffer")
   "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'dired
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   )
  )

(use-package ivy :ensure t
  :diminish (ivy-mode . "")             ; does not display ivy in the modeline
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


(use-package counsel :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC f f" 'counsel-find-file
   "SPC h f" 'counsel-describe-function
   "SPC u"   'counsel-unicode-char
   "SPC p f" '(counsel-git :which-key "find file in git dir")
   "SPC p s" 'counsel-rg
   "SPC SPC" 'counsel-M-x))

(use-package swiper :ensure t
  :general
  (general-define-key
   :keymaps 'normal
      "SPC s" 'swiper))


;; display help for key usage
(use-package which-key :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package avy :ensure t
  :commands (avy-goto-word-1))

(use-package flycheck :ensure t
             :config
             (global-flycheck-mode))

(use-package ranger :ensure t
             :commands (ranger)
             :bind (("C-x d" . deer))
             :config
             (setq ranger-cleanup-eagerly t)
             )
(use-package git-gutter :ensure t
             :config
             (global-git-gutter-mode +1))
;;----------------------------------------------------------------------------
;; evil
;;----------------------------------------------------------------------------
(use-package evil
  :ensure t ;; install the evil package if not installed
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
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

;; Press “%” to jump between matched tags
(use-package evil-matchit
  :ensure t
  :config
    (global-evil-matchit-mode))

(use-package evil-nerd-commenter
  :ensure t
  :general
  ("M-/" 'evilnc-comment-or-uncomment-lines))

(use-package powerline
  :ensure t
  :config
    (powerline-center-evil-theme))

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



;;----------------------------------------------------------------------------
;; misc
;;----------------------------------------------------------------------------
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
    (setq aggressive-indent-sit-for-time 0.5))

(use-package magit :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC g s" 'magit-status))

(use-package evil-magit :ensure t
  :init
  (setq evil-magit-state 'normal)
    (setq evil-magit-use-y-for-yank nil)
  )

;; Highlighting TODO keywords
(use-package hl-todo
  :ensure t
    :config (global-hl-todo-mode))

;; rss feed
(use-package elfeed-goodies :ensure t)
(use-package elfeed-web :ensure t)
(use-package elfeed
  :ensure t
  :config
    (setq elfeed-db-directory (expand-file-name "feeds" user-emacs-directory)))

;;----------------------------------------------------------------------------
;; languages
;;----------------------------------------------------------------------------

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; haskell
(use-package haskell-mode
  :ensure t
  :config
    (setq haskell-interactive-popup-error nil))

(use-package lua-mode
    :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package less-css-mode
  :ensure t
    :mode "\\.less\\'")

;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

