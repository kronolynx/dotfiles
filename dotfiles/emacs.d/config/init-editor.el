(setq delete-old-versions -1; delete excess backup versions silently
      version-control t; use version control
      vc-make-backup-files t; make backups file even when in version controlled dir
      backup-directory-alist `(("." . "~/.emacs.d/backups")) ; which directory to put backups
      vc-follow-symlinks t       ; don't ask for confirmation when opening symlinked file
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ;transform backups file name
      sentence-end-double-space nil ; sentence SHOULD end with only a point.
      custom-safe-themes t
      )
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


;; replace the active region just by typing text, just like modern editors
(delete-selection-mode +1)

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
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; remember last position in file
(save-place-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)
;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t)

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

(setq save-place-forget-unreadable-files nil
      ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      scroll-preserve-screen-position t)




(provide 'init-editor)
