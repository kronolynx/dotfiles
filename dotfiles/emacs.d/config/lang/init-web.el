(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))

(use-package rjsx-mode
  :disabled
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package typescript-mode
  :disabled
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))))

(use-package sass-mode
  :disabled
  :mode "\\.sass\\'")

(use-package less-css-mode
  :disabled
  :mode "\\.less\\'")

(provide 'init-web)
