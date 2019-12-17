;; ;; haskell-mode configuration
;; ;; https://github.com/haskell/haskell-mode

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends)
		   (append '((company-capf company-dabbrev-code))
			   company-backends))))
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

(provide 'lang-haskell)
