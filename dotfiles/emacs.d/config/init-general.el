(global-unset-key (kbd "C-/"))
(use-package general
  :config
  (general-unbind "C-/")
  (general-define-key
   "C-;" 'avy-goto-word-1
   "C-s" 'save-all
   "C-/" 'evilnc-comment-or-uncomment-lines
   "M-/" 'evilnc-comment-or-uncomment-lines
   "M-1"  'neotree-toggle
   )
  (general-def 'normal 'neotree-mode
    "z" 'neotree-toggle
    "RET" 'neotree-enter
    "c"   'neotree-create-node
    "r"   'neotree-rename-node
    "d"   'neotree-delete-node
    "j"   'neotree-next-node
    "k"   'neotree-previous-node
    "SPC" 'neotree-change-root
    "q"   'neotree-hide
    "l"   'neotree-enter    )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; simple command
   "/"   'counsel-ag
   "TAB" '(switch-to-other-buffer :which-key "prev buffer")
   "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
   ;; neotree
   "nt" 'neotree-toggle
   ;; buffers
   "bl" 'ivy-switch-buffer
   ;; files
   "ff" 'counsel-find-file
   ;; search
   "sc" 'evil-ex-nohighlight

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'dired
   "g"  '(:ignore t :which-key "Git")
   ;; Git
   "ms" '(magit-status :which-key "git status")
   "ml" '(magit-log :which-key "git log")
   "md" '(magit-diff :which-key "git diff")
   "mc" '(magit-commit :which-key "git commit")
   "mf" '(magit-fetch :which-key "git fetch")
   )
  )

(provide 'init-general)
