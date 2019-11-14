(use-package sh-script                  ; Shell scripts
  :defer t
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        )
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package fish-mode
  :defer t
  :mode "\\.fish\\'")

(use-package company-shell
  :defer t
  :after company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :hook
  (eshell-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-shell company-shell-env company-fish-shell))))
  (sh-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-shell company-shell-env company-fish-shell))))
  )

(provide 'init-sh)
