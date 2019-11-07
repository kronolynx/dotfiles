;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------
(eval-and-compile
  (require 'package))
(setq package-archives '(
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         )
      )
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; global ensure
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
;;(setq use-package-always-defer t
;;      use-package-always-ensure t
;;      )

(provide 'init-package)
