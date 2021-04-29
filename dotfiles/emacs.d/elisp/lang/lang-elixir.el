(use-package alchemist
  :defer t
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package flycheck-mix
  :after alchemist
  :commands (flycheck-mix-setup))

(use-package elixir-mode
  :defer t)

(provide 'lang-elixir)
