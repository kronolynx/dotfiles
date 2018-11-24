(require 'package)
(setq package-archives '(
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         )
      )

(package-refresh-contents)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(bind-keys*
                                        ;("M-%" . query-replace-regexp)
                                        ;("M-`" . other-frame)
                                        ;("M-z" . just-one-space)
 ("C-M-;" . comment-or-uncomment-region)
                                        ;("M-M" . man)
                                        ;("C-M-k" . kill-sexp)
                                        ;("C-<tab>" . mode-line-other-buffer)
                                        ;("C-<tab>" . mode-line-other-buffer)
                                        ;("C-S-<tab>" . next-buffer)
 ("C-;" . comment-line)
 ("C-/" . comment-line)
                                        ;("C-c q" . delete-other-windows)
 )

(add-to-list
 'custom-theme-load-path
 (expand-file-name "themes" user-emacs-directory))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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

(setq custom-file (make-temp-file "emacs-custom-")
      custom-buffer-done-kill nil
      custom-buffer-verbose-help nil
      custom-unlispify-names nil
      custom-unlispify-menu-entries nil

      initial-scratch-message nil

      custom-safe-themes t
      enable-local-variables t

      frame-title-format '("" "%b @ Emacs " emacs-version)

      gc-cons-threshold 100000000
      large-file-warning-threshold 100000000

      load-prefer-newer t
      require-final-newline t

      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil

      make-backup-files nil

      confirm-kill-emacs nil

      echo-keystrokes 0.2
      ring-bell-function #'ignore

      set-mark-command-repeat-pop t

      inhibit-default-init t
      inhibit-startup-screen t

      scroll-margin 2
      scroll-preserve-screen-position t)

(put 'var 'safe-local-variable (lambda [& rest] true))

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))))
  (set-register (car r) (cdr r)))

(bind-key* "C-x r j" 'jump-to-register)

(dolist (cmd '(narrow-to-region
               narrow-to-page
               narrow-to-defun
               upcase-region
               downcase-region
               erase-buffer
               eval-expression
               dired-find-alternate-file
               set-goal-column))
  (put cmd 'disabled nil))

(when (display-graphic-p)
  (unbind-key "C-z"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(unless (and (display-graphic-p)
             (eq system-type 'darwin))
  (menu-bar-mode -1))
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)


;; Latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init)
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  )


(add-hook 'focus-out-hook 'save-all)

(global-set-key [(meta ctrl j)] 'transpose-lines)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "M-D") 'duplicate-line)

(use-package replace
  :config (setq case-replace t
                case-fold-search nil))

(use-package paren
  :config (show-paren-mode))

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-<return>" . isearch-done-opposite)
              ("M-i" . helm-swoop-from-isearch))
  :init (defun isearch-done-opposite (&optional nopush edit)
          "End current search in the opposite side of the match."
          (interactive)
          (funcall #'isearch-done nopush edit)
          (when isearch-other-end (goto-char isearch-other-end))))

(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package hi-lock
  :diminish hi-lock-mode)

(use-package saveplace
  :config (save-place-mode))

(use-package delight
  :ensure t)

(use-package diminish
  :ensure t)

(use-package discover
  :ensure t
  :config (global-discover-mode))

(use-package move-text
  :ensure t
  :bind* (("M-<up>" . move-text-up)
          ("M-<down>" . move-text-down)))

(use-package org
  :pin org
  :config (setq org-src-fontify-natively t))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; make org source smaller
(set-face-attribute 'org-meta-line nil :height 0.7 :slant
                    'normal :foreground "#C0C0C0" )
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
;; (setq org-hide-block-startup t)
(setq org-image-actual-width (/ (display-pixel-width) 3) )
                                        ;(use-package rainbow-mode
                                        ;:load-path "vendor")

(use-package gitconfig-mode
  :ensure t)

(use-package toc-org
  :ensure t)

;; Install fonts for icons
;; M-x all-the-icons-install-fonts
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t))

(use-package discover-my-major
  :ensure t
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-S-m" . discover-my-mode)))

(use-package ediff
  :config
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  (setq ediff-diff-options "-w")

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise)))

(use-package sh-script
  :mode "\\.sh\\'"
  :config (setq sh-indentation 2 sh-basic-offset 2))

(use-package man
  :config (setq Man-width 79)
  :defer t)

(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode))

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask" . emacs-lisp-mode)))

(use-package whitespace
  :bind (("C-x S" . whitespace-cleanup-save-buffer))
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :init
  (defun whitespace-cleanup-save-buffer ()
    (interactive)
    (whitespace-cleanup)
    (save-buffer))
  :hook (prog-mode . whitespace-mode)
  :config
  (setq-default whitespace-style '(face trailing tab-mark)))

(use-package electric
  :hook (prog-mode . electric-indent-mode))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :hook ((rust-mode) . flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :config (setq rust-format-on-save t))

(use-package company
  :ensure t
  :hook ((typescript-mode . company-mode)
         (racer-mode . company-mode)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

(use-package emmet-mode
  :ensure t
  :hook (((typescript-mode web-mode rjsx-mode) . emmet-mode)))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((racer-mode . eldoc-mode)))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; $ rustup component add rust-src
;; $ cargo install racer
(use-package racer
  :ensure t
  :hook (rust-mode . racer-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck eldoc))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook
   'typescript-mode-hook
   (defun setup/typescript ()
     (interactive)
     (tide-setup)
     (tide-hl-identifier-mode)
     (eldoc-mode)
     (add-hook 'before-save-hook 'tide-format-before-save))))

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-'" . avy-goto-char)
         ("C-M-'" . avy-goto-char-2)))

(use-package hl-todo
  :ensure t
  :demand t
  :bind (("C-c t n" . hl-todo-next)
         ("C-c t p" . hl-todo-previous)
         ("C-c t o" . hl-todo-occur))
  :config (global-hl-todo-mode))

(use-package ace-window
  :ensure t
  :init (defun other-window-backwards ()
          (interactive)
          (other-window -1))
  :bind (("M-o" . other-window)
         ("M-O" . other-window-backwards)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package multi-term
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package subword
  :diminish subword-mode
  :hook ((haskell-mode
          clojure-mode
          scala-mode
          elm-mode
          js2-mode
          json-mode
          typescript-mode
          web-mode) . subword-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :hook ((haskell-mode scala-mode elm-mode js2-mode typescript-mode) . hungry-delete-mode))

(use-package sml-mode
  :ensure t
  :mode "\\.sml\'")

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :hook ((haskell-mode-hook . haskell-doc-mode)
         (haskell-mode-hook . turn-on-haskell-indentation))
  :init
  (setq haskell-stylish-on-save t)

  (defun haskell-mode-before-save-handler ()
    "Function that will be called before buffer's saving."
    (when (projectile-project-p)
      (haskell-mode-stylish-buffer)
      (haskell-sort-imports)))
  )

(use-package nand2tetris
  :ensure t
  :mode ("\\.hdl\\'" . nand2tetris-mode))

(use-package gitignore-mode
  :ensure t
  :mode ".gitignore'")

(use-package git-messenger
  :ensure t
  :bind ("C-x g p" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t
  :bind ("C-x g t" . git-timemachine-toggle))

(use-package what-the-commit
  :ensure t
  :bind ("C-x g c" . what-the-commit-insert))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ('prog-mode-hook #'rainbow-delimiters-mode))

(use-package edit-server
  :ensure t
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

(use-package magit
  :ensure t
  :pin melpa-stable
  :hook (magit-mode . hl-line-mode)
  :config
  (use-package magit-popup :ensure t :pin melpa)
  (when (functionp 'ivy-completing-read)
    (setq magit-completing-read-function 'ivy-completing-read)))

(use-package magit-todos
  :ensure t
  :hook (magit-mode . magit-todos-mode))

(use-package gist
  :ensure t)

(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config (setq-default json-reformat:indent-width 2
                        js-indent-level 2))

(use-package osx-trash
  :ensure t
  :if (eq system-type 'darwin)
  :config (osx-trash-setup))

(use-package dash-at-point
  :ensure t
  :if (and (display-graphic-p)
           (eq system-type 'darwin))
  :bind (("C-c d f" . dash-at-point)
         ("C-c d e" . dash-at-point-with-docset)))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package tldr :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md" . gfm-mode)
         ("\\.markdown" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package python-mode
  :mode "\\.py\\'"
  :init (setq python-indent-offset 2))

(use-package yasnippet
  :ensure t
  :defer 2
  :diminish (yas-minor-mode yas-global-mode)
  :init (setq yas-indent-line t)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'")

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :init
  (setq elm-format-on-save t))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package gradle-mode
  :ensure t
  :mode "\\.gradle\\'")

(use-package server
  :config
  (unless (server-running-p) (server-mode)))

(use-package fancy-narrow
  :ensure t
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode))

(use-package dired-explorer
  :ensure t)

(use-package dired-imenu
  :ensure t)

(use-package mac
  :if (eq system-type 'darwin))

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

;; (use-package centered-window
;;   :ensure t
;;   :config (setq cwm-incremental-padding t
;;                 cwm-incremental-padding-% 2))

(use-package untitled-new-buffer
  :ensure t
  :bind (("M-n" . untitled-new-buffer-with-select-major-mode)))

(use-package helm
  :ensure t
  :pin melpa-stable
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-S-b" . ibuffer)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)

         ("C-x c !" . helm-calcul-expression)
         ("M-:" . helm-eval-expression-with-eldoc)

         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-c h i" . helm-semantic-or-imenu)
         )
  :config
  (setq helm-command-prefix-key "C-c h"
        helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-buffer-max-length nil
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-ff-auto-update-initial-value t
        helm-full-frame nil
        )
  (helm-mode))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-ag
  :ensure t
  :defer t)

(use-package helm-tramp
  :ensure t
  :defer t)

(use-package helm-themes
  :ensure t
  :bind ("C-c h t" . helm-themes))

(use-package helm-swoop
  :ensure t
  :pin melpa-stable
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop)))

(use-package helm-projectile
  :ensure t
  :bind* (("C-c p D" . projectile-dired)
          ("C-c p v" . projectile-vc)
          ("C-c p k" . projectile-kill-buffers)

          ("C-c p p" . helm-projectile-switch-project)
          ("C-c p f" . helm-projectile-find-file)
          ("C-c p F" . helm-projectile-find-file-in-known-projects)
          ("C-c p g" . helm-projectile-find-file-dwin)
          ("C-c p d" . helm-projectile-find-dir)
          ("C-c p C-r" . helm-projectile-recentf)
          ("C-c p b" . helm-projectile-switch-to-buffer)
          ("C-c p s s" . helm-projectile-ag)
          ("C-c p s g" . helm-projectile-grep)
          )
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode)
  (helm-projectile-on))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window nil)
  (fullframe projectile-vc magit-mode-quit-window nil))

(use-package golden-ratio
  :ensure t
  :delight
  :config (golden-ratio-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook ((clojure-mode emacs-lisp-mode) . aggressive-indent-mode)
  :config
  (setq aggressive-indent-sit-for-time 0.5))

(use-package paredit
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode)
  :config
  :diminish (paredit paredit-mode))

(use-package prog-mode
  :hook
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
  :config
  (setq clojure--prettify-symbols-alist '(("fn" . 955)
                                          )))

(use-package elfeed-goodies :ensure t)
(use-package elfeed-web :ensure t)
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "feeds" user-emacs-directory)))

(use-package idris-mode
  :ensure t)

(use-package helm-idris
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package cider
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c C-;" . cider-eval-defun-to-comment)
              ("C-c C-SPC" . cider-format-buffer)))
(use-package helm-cider
  :ensure t
  :config (eval-after-load "cider" (helm-cider-mode)))

(use-package clj-refactor
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-n")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
  (setq clojure-indent-style ':align-arguments
        cider-cljs-lein-repl (paredit-unescape-string
                              (with-output-to-string
                                (print '(cond
                                         (and (resolve 'user/run)
                                              (resolve 'user/browser-repl))
                                         (eval '(do (user/run)
                                                    (user/browser-repl)))

                                         (try
                                          (require '[figwheel-sidecar.repl-api :as sidecar])
                                          (resolve 'sidecar/start-figwheel!)
                                          (catch Throwable _))
                                         (eval '(do (sidecar/start-figwheel!)
                                                    (sidecar/cljs-repl)))

                                         (try
                                          (require '[cemerick.piggieback :as piggie])
                                          (resolve 'piggie/cljs-repl)
                                          (catch Throwable _))
                                         (eval '(piggie/cljs-repl (cljs.repl.rhino/repl-env)))

                                         :else
                                         (throw (ex-info "Failed to initialize CLJS repl.")))))))
  (define-clojure-indent
    (defcomponent '(2 nil nil (:defn)))))

(use-package hideshow
  :hook (clojure-mode . hs-minor-mode))

(use-package eshell-prompt-extras
  :ensure t
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package nyan-mode :ensure t)

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

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook (defun my/java-mode-setup ()
                              (setq c-basic-offset 2
                                    tab-width 2
                                    indent-tabs-mode nil))))


(use-package scala-mode
  :ensure t
  :mode (("\\.scalaX?\\'" . scala-mode)
         ("\\.scX?\\'" . scala-mode)))

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package remember-last-theme
  :ensure t
  :config (remember-last-theme-with-file-enable (expand-file-name "last-theme.el" user-emacs-directory)))

(use-package rtags :ensure t)
(use-package cmake-ide
  :ensure t
  :config (cmake-ide-setup))

(use-package smart-window
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "ZSH" "MANPATH"
                                         "SSH_AUTH_SOCK"
                                         "SSH_AGENT_PID"
                                         "GPG_AGENT_INFO"
                                         "GNOME_KEYRING_CONTROL"
                                         "GNOME_KEYRING_PID"))
  (exec-path-from-shell-initialize))

(use-package sicp :ensure t)

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-search-module 'evil-search)
  (setq evil-shift-round nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-integration nil)
  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; ;; change mode-line color by evil state
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let ((color (cond ((minibufferp) default-color)
;;                                  ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;;                                  ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                                  ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                                  (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-foreground 'mode-line (cdr color))))))

(use-package evil-escape
  :ensure t
  ;; :load-path "vendor"
  :config
  (setq evil-escape-lighter '())
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "yy"
                evil-escape-delay 0.2))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

;; (use-package linum-relative
;;   :ensure t
;;   :hook (prog-mode . linum-relative-mode)
;;   :init
;;   ;; TODO linum progn is not working
;;   (progn
;;     (setq linum-relative-format "%3s ")
;;     ;; display current line instead of 0
;;     (setq linum-relative-curren-symbol "")))

(use-package evil-nerd-commenter
  :ensure t
  :init
  (progn
    (evilnc-default-hotkeys)
    ;; default binding is M-;
    (global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)))

(use-package evil-leader
  :ensure t
  :init
  (progn
    (global-evil-leader-mode 1)
    (evil-leader/set-leader "SPC")))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package ace-jump-mode
  :ensure t
  :init
  (progn
    (evil-leader/set-key "SPC" 'evil-ace-jump-char-mode)))

(define-key evil-normal-state-map "gB" 'evil-next-buffer);next buffer
(define-key evil-normal-state-map "gb" 'evil-prev-buffer);previous buffer

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; save buffers on focus lost
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(use-package afternoon-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package avk-emacs-themes :ensure t :defer t)
(use-package bliss-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package clues-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package jazz-theme :ensure t :defer t)
(use-package madhat2r-theme :ensure t :defer t)
(use-package molokai-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package night-owl-theme :ensure t :defer t)
(use-package noctilux-theme :ensure t :defer t)
(use-package paganini-theme :ensure t :defer t)
(use-package railscasts-theme :ensure t :defer t)
(use-package rebecca-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package spacegray-theme :ensure t :defer t)
(use-package subatomic-theme :ensure t :defer t)
