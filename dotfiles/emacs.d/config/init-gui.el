
(scroll-bar-mode -1)
(tool-bar-mode -1)  
(blink-cursor-mode -1)
(transient-mark-mode -1)
(delete-selection-mode)
(column-number-mode)
(tooltip-mode    -1)
(menu-bar-mode   -1)


(setq ring-bell-function 'ignore ; silent bell when you make a mistake
      )

(add-hook
 'after-init-hook
 (defun my/set-faces ()
   (custom-set-faces
    '(default ((t (:height 130 :family "SauceCodePro Nerd Font" :weight normal)))))
   ))


(provide 'init-gui)
