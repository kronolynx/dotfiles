(use-package lua-mode
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :custom
  (lua-indent-level 2)
  (lua-indent-string-contents t)
  )

(provide 'init-lua)
