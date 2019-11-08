;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------
;; (eval-and-compile
;;   (require 'package))
;; (setq package-archives '(
;;                          ("elpa" . "https://elpa.gnu.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          )
;;       )
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package)
;;   (package-install 'diminish))

(eval-and-compile
  (setq load-prefer-newer t
        package--init-file-ensured t
        package-enable-at-startup nil)
  )

(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key)))

(eval-when-compile
  (require 'use-package)
  (require 'diminish))

;; global ensure
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
;;(setq use-package-always-defer t
;;      use-package-always-ensure t
;;      )

(provide 'init-package)
