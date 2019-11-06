;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/lang" user-emacs-directory))

(require 'base)
(require 'base-evil) ;; should be loaded before other packages
(require 'base-theme)
(require 'base-keys)
(require 'base-extensions)

(require 'init-org)
(require 'init-lang)
