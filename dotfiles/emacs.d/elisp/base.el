(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------
;; (setq
;;  ;; avoid checking packages on startup to speed it up
;;  straight-check-for-modifications '(check-on-save))
(setq straight-check-for-modifications 'live)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; supress warning Package cl is deprecated in Emacs-27
(setq byte-compile-warnings '(cl-functions))
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

;; Suppress “ad-handle-definition: .. redefined” warnings during Emacs startup
(customize-set-variable 'ad-redefinition-action 'accept)
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
      x-select-enable-clipboard          t)

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

(setq ad-redefinition-action 'accept)
;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(defun my/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font" :height 110)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "FantasqueSansMono Nerd Font" :height 110)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "FantasqueSansMono Nerd Font" :height 110 :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		;;(setq doom-modeline-icon t)
                (with-selected-frame frame
                  (my/set-font-faces))))
  (my/set-font-faces))


(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(require 'server)
(if (not (server-running-p)) (server-start))

;; When two buffers are open with the same name, this makes it easier to tell them apart.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'base)
;;; base ends here
