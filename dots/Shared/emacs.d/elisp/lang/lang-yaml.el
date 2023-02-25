(use-package yaml-mode                  ; YAML
  :defer t
  :mode ("\\.yml$" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate)
              (run-hooks 'prog-mode-hook))))

(provide 'lang-yaml)
