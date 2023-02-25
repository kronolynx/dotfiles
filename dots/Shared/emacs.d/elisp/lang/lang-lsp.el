(use-package lsp-mode
  :defer t
  ;; Optional - enable lsp-mode automatically in scala files
  :custom
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  :hook
  ((scala-mode c-mode c++-mode python-mode) . lsp)
  (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  )

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :after lsp-mode
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui
  :after lsp-mode
  )

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :defer t
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))


;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  :after lsp-mode
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(provide 'lang-lsp)
