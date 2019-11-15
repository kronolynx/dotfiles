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
(require 'init-org)

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

(use-package ivy-rich
  :defer 0.1
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))

  (defun ivy-rich-compiling (candidate)
    "Displays compiling buffers of the candidate for ivy-rich."
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
              (not (magit-git-repo-p candidate)))
          ""
        (if (my/projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))

  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate               (:width 73))
                (ivy-rich-file-user               (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size               (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate        (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon       (:width 2))
                (ivy-rich-candidate                (:width 40))
                (ivy-rich-switch-buffer-size       (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package counsel
  :after ivy
  :delight
  :config (counsel-mode)
  )

(use-package swiper
  :after ivy)
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
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-enable-caching t)
  (projectile-remember-window-configs t)
  (projectile-use-git-grep 1)
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

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


(use-package docker-tramp)

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "firefox"))

(use-package engine-mode
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.es/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

(use-package calc
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  (math-units-table nil))


(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  ;; (savehist-file (expand-file-name (format "%s/emacs/history" xdg-cache)))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))

(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

(use-package wiki-summary
  :defer 1
  :preface
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, sticks it in the *wiki-summary* buffer and displays
     the buffer."
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf))))

(advice-add 'wiki-summary/format-summary-in-buffer :override #'my/format-summary-in-buffer)

(use-package faces
  :ensure nil
  :custom (show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#262b36")
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))

(use-package webpaste :defer 1)
