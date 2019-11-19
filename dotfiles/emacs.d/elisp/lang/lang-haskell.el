;; haskell-mode configuration
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :custom
  (add-to-list 'completion-at-point-functions     'haskell-process-completions-at-point)
  (haskell-align-imports-pad-after-name            t) ;; nil)
  (haskell-ask-also-kill-buffers                   nil)
  (haskell-completions-complete-operators          nil)
  (haskell-compile-cabal-build-command            "stack build -j 7 --fast --ghc-options=\"-j +RTS -A32M -RTS\"")
  ;; (haskell-hoogle-url                             "https://hoogle.haskell.org/?hoogle=%s")
  (haskell-hoogle-command                          "hoogle --color -l --numbers --count=10")
  (haskell-indentation-electric-flag               t)
  (haskell-interactive-mode-eval-mode             'haskell-mode)
  (haskell-interactive-mode-hide-multi-line-errors nil)
  (haskell-interactive-mode-include-file-name      nil)
  (haskell-interactive-mode-scroll-to-bottom       t)
  (haskell-interactive-types-for-show-ambiguous    t) ;; check
  (haskell-interactive-popup-errors                nil)
  (haskell-mode-stylish-haskell-path              "stylish-haskell")
  (haskell-notify-p                                t)
  (haskell-process-args-cabal-repl                '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (haskell-process-args-cabal-new-repl            '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (haskell-process-args-ghci                      '("-ferror-spans" "-fshow-loaded-modules"))
  (haskell-process-args-stack-ghci                '("--ghci-options=-ferror-spans"
						    "--ghci-options=-fshow-loaded-modules"
						    "--ghci-options=-fno-diagnostics-show-caret"
						    "--no-build"
						    "--no-load"
						    ))
  (haskell-interactive-set-+c                     nil) ;; testing
  (haskell-process-auto-import-loaded-modules      t) ;;nil)
  (haskell-process-do-cabal-format-string         ":!cd %s && %s")
  (haskell-process-load-or-reload-prompt           nil)
  (haskell-process-log                             t)
  (haskell-process-path-ghci                       "stack ghci")
  (haskell-process-reload-with-fbytecode           t) ;; nil)
  (haskell-process-suggest-haskell-docs-imports    t)
  (haskell-process-suggest-hoogle-imports          t)
  (haskell-process-suggest-hayoo-imports           nil)
  (haskell-process-suggest-remove-import-lines     t)

  (haskell-process-suggest-add-package             t)
  (haskell-process-type                           'stack-ghci)
  (haskell-process-use-presentation-mode           t)
  (haskell-process-show-debug-tips nil)
  (haskell-stylish-on-save                         nil) ;; twice undo-tree saving
  (haskell-tags-on-save                            nil) ;; using projectile with codex every 30 secs
  (haskell-complete-module-preferred
   '("ClassyPrelude" "Data.Conduit" "Data.Function" "Data.List" "Data.Map"))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-files))))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  ;; hindent - format haskell code automatically
  ;; https://github.com/chrisdone/hindent
  (when (executable-find "hindent")
    (use-package hindent
      :diminish hindent-mode
      :config
      (add-hook 'haskell-mode-hook #'hindent-mode)
      ;; reformat the buffer using hindent on save
      (setq hindent-reformat-buffer-on-save t)))
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

;; Prepare auto completion with company
(use-package company-ghci
  :config
  (eval-after-load 'company-mode (add-to-list 'company-backends 'company-ghci))
  )


;; Prepare flyCheck
(use-package flycheck-haskell
  ;; :bind (:map haskell-mode-map
  ;; 	      ("M-n" . 'flycheck-next-error)
  ;; 	      ("M-p" . 'flycheck-previous-error)
  ;; 	      )
  :init (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
  )

;; Setup auto-completion with company
(use-package company-cabal
  :config
  (eval-after-load 'company-mode (add-to-list 'company-backends 'company-cabal))
  )


(provide 'lang-haskell)
