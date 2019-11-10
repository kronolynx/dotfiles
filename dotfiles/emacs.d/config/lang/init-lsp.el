(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :custom
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  :hook
  ((scala-mode) . lsp)
  :config
  (require 'lsp-clients)
  )

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp
  :after company
  )

(provide 'init-lsp)
