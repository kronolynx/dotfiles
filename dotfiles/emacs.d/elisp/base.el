;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------

(eval-and-compile
  (setq load-prefer-newer t
        package--init-file-ensured t
        package-enable-at-startup nil)
  )

(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key)))

(eval-when-compile
  (require 'use-package)
  (require 'diminish))

;; global ensure
(require 'use-package-ensure)
(setq use-package-always-ensure t
      ;; use-package-always-defer t
      )

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")


;;----------------------------------------------------------------------------
;; Core settings
;;----------------------------------------------------------------------------
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      indent-tabs-mode                   nil
      inhibit-startup-message            t
      fringes-outside-margins            t
      x-select-enable-clipboard          t
      use-package-always-ensure          t)

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq
 history-length                     1000
 backup-inhibited                   nil
 find-file-visit-truename t
 vc-follow-symlinks t
 make-backup-files                  t
 auto-save-default                  t
 save-interprogram-paste-before-kill t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  t
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(unless (file-exists-p (concat temp-dir "/auto-save-list"))
  (make-directory (concat temp-dir "/auto-save-list") :parents))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(show-paren-mode 1)

;; replace the active region just by typing text, just like modern editors
(delete-selection-mode +1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; save buffers on focus lost
(defun save-all ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(global-prettify-symbols-mode +1)
(setq inhibit-compacting-font-caches t)

;; scroll with wheel on terminal
(xterm-mouse-mode t)
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   1)))

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

(setq save-place-forget-unreadable-files nil
      ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      scroll-preserve-screen-position t)
;; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)
;; remember last position in file
(save-place-mode 1)

;;;; Quiet Startup
;;(setq inhibit-startup-screen t)
;;(setq inhibit-startup-message t)
;;(setq inhibit-startup-echo-area-message t)
;;(setq initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-hook
 'after-init-hook
 (defun my/set-faces ()
   (custom-set-faces
    '(default ((t (:height 110 :family "SauceCodePro Nerd Font" :weight normal)))))
   ))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(provide 'base)
;;; base ends here
