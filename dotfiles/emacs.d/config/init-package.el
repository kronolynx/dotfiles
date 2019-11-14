;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------

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
(setq use-package-always-ensure t
      ;; use-package-always-defer t
      )

(provide 'init-package)
