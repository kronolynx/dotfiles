
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)

;; remember last position in file
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

(add-hook
 'after-init-hook
 (defun my/set-faces ()
   (custom-set-faces
    '(default ((t (:height 130 :family "SauceCodePro Nerd Font" :weight normal)))))
   ))

(provide 'init-gui)
