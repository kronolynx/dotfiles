;;----------------------------------------------------------------------------
;; languages
;;----------------------------------------------------------------------------

;; haskell
(use-package haskell-mode
  :config
  (setq haskell-interactive-popup-error nil))

(use-package lua-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))

(use-package rjsx-mode
  :disabled
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package typescript-mode
  :disabled
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))))

(use-package sass-mode
  :disabled
  :mode "\\.sass\\'")

(use-package less-css-mode
  :disabled
  :mode "\\.less\\'")

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (progn (add-hook 'scala-mode-hook #'yas-minor-mode)))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

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

(use-package yaml-mode                  ; YAML
  :defer t
  :mode ("\\.yml$" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate)
              (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :defer t
  :config
  (progn                                ; https://github.com/skeeto/.emacs.d
    (setf json-reformat:pretty-string? t
          json-reformat:indent-width 2)
    (define-key json-mode-map (kbd "M-q")
      (lambda ()
        (interactive)
        (if (region-active-p)
            (call-interactively #'json-reformat-region)
          (json-reformat-region (point-min) (point-max)))))

    (add-hook 'json-mode-hook
              ;; Fix JSON mode indentation
              (lambda () (setq-local js-indent-level 4)))))

(use-package json-reformat              ; Reformat JSON
  :defer t
  :bind (("C-c x j" . json-reformat-region)))


(use-package sh-script                  ; Shell scripts
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package fish-mode
  :mode "\\.fish\\'")

(provide 'init-prog-lang)
