(require 'init-elpa)
(require-package 'evil)
(require-package 'evil-matchit)
(require-package 'evil-snipe)
(require-package 'evil-surround)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(provide 'init-evil)
