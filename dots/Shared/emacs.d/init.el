;;; package --- sumary
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; code:
;;
;;   ██ ▄█▀ ██▀███   ▒█████   ███▄    █  ▒█████
;;   ██▄█▒ ▓██ ▒ ██▒▒██▒  ██▒ ██ ▀█   █ ▒██▒  ██▒
;;  ▓███▄░ ▓██ ░▄█ ▒▒██░  ██▒▓██  ▀█ ██▒▒██░  ██▒
;;  ▓██ █▄ ▒██▀▀█▄  ▒██   ██░▓██▒  ▐▌██▒▒██   ██░
;;  ▒██▒ █▄░██▓ ▒██▒░ ████▓▒░▒██░   ▓██░░ ████▓▒░
;;  ▒ ▒▒ ▓▒░ ▒▓ ░▒▓░░ ▒░▒░▒░ ░ ▒░   ▒ ▒ ░ ▒░▒░▒░
;;  ░ ░▒ ▒░  ░▒ ░ ▒░  ░ ▒ ▒░ ░ ░░   ░ ▒░  ░ ▒ ▒░
;;  ░ ░░ ░   ░░   ░ ░ ░ ░ ▒     ░   ░ ░ ░ ░ ░ ▒
;;  ░  ░      ░         ░ ░           ░     ░ ░
;;
;;
(let ((file-name-handler-alist nil)) ;; optimize startup time
  (add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "elisp/lang" user-emacs-directory))

  (if (member "-M" command-line-args)
      (progn
	;; Abort and load minimal init instead
	;; This is useful if we are running in a resource constrained
	;; environment or have broken the main config
	(delete "-M" command-line-args)
	(load (locate-user-emacs-file "init-minimal"))))

  (require 'base)
  (require 'base-theme)
  (require 'base-keys)
  (require 'base-evil) ;; should be loaded before other packages
  (require 'base-extensions)
  (require 'init-lang))
;;(require 'init-org)
