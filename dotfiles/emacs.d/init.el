;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/lang" user-emacs-directory))

(require 'base)
(require 'base-theme)
(require 'base-extensions)
(require 'base-keys)

(require 'init-evil)
(require 'init-org)
(require 'init-lang)
