;;;  init.el --- Where all the magic begins
;;
;; My emacs configuration
;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-evil)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-miscellaneous)
(require 'init-company-mode)
;;(require 'init-rust)
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (
     atom-one-dark-theme
     company
     evil-matchit
     evil-snipe
     evil-surround
     flycheck
     golden-ratio
     ido-completing-read+
     projectile
     rainbow-delimiters
     smex linum-relative
     ))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
