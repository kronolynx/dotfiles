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
  (general-define-key '
   :states '(scala-mode)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "ei" 'lsp-metals-build-connect
   )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; simple command
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "prev buffer")
   "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
   ;; neotree
   "nt" 'neotree-toggle
   ;; buffers
   "t" '(:ignore t :which-key "buffer")
   "tl" '(counsel-buffer-or-recentf :which-key "switch or recent")
   "to" '(counsel-switch-buffer-other-window :which-key "other window")
   ;; files
   "f" '(:ignore t :which-key "file")
   "ff" '(counsel-find-file :which-key "find")
   "fr" '(counsel-recentf :which-key "recent")
   ;; search
   "s" '(:ignore t :which-key "search")
   "ss" '(counsel-grep-or-swiper :which-key "swiper")
   "sp" '(swiper-thing-at-point :which-key "swiper at point")
   "se"   '(counsel-ag :which-key "everywhere")
   ;; "sc" 'evil-ex-nohighlight

   ;; goto
   "g" '(:ignore t :which-key "goto")

   "x" '(:ignore t :which-key "xref")
   "xo" '(xref--find-definitions-other-window :which-key "go to definition other")
   "xg" '(xref--find-definitions :which-key "go to definition")

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'dired
   ;; Git
   "m" '(:ignore t :which-key "git")
   "ms" '(magit-status :which-key "git status")
   "ml" '(magit-log :which-key "git log")
   "md" '(magit-diff :which-key "git diff")
   "mc" '(magit-commit :which-key "git commit")
   "mf" '(magit-fetch :which-key "git fetch")
   )
  )

(provide 'init-general)
