(require 'init-elpa)
(require-package 'company)
(require-package 'flycheck)
(require-package 'racer)
(require-package 'rust-mode)
(require-package 'cargo)
(require-package 'flycheck-rust)

(require 'company)
(require 'electric)
(require 'eldoc)
(require 'racer)
(require 'rust-mode)
(require 'flycheck-rust)
(require 'cargo)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'company-mode)
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          '(lambda ()
             (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
             (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (setq company-tooltip-align-annotations t)
             (setq rust-format-on-save t)
             (electric-pair-mode 1)))



(provide 'init-rust)
