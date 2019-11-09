(global-unset-key (kbd "C-/"))
(use-package general
  :config
  (general-unbind "C-/")
  (general-define-key
   "C-;" 'avy-goto-word-1
   "C-s" 'save-all
   "C-/" 'evilnc-comment-or-uncomment-lines
   "M-/" 'evilnc-comment-or-uncomment-lines
   "M-1" 'neotree-toggle
   "M-n" 'counsel-M-x
   "C->" '(evil-window-decrease-width :which-key "decrease width")
   "C-<" '(evil-window-increase-width :which-key "increase width")
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
   "se" '(counsel-ag :which-key "everywhere")
   "sr" '(counsel-rg :which-key "rip-grep")
   "sg" '(counsel-git :which-key "git")

   ;; editor
   "e" '(:ignore t :which-key "editor")
   "ed" '(counsel-describe-function :whiche-key "describe function")
   "u"   '(counsel-unicode-char)
   "pf" '((counsel-git :which-key "find file in git dir"))
   ;; goto
   "g" '(:ignore t :which-key "goto")

   "x" '(:ignore t :which-key "xref")
   "xo" '(xref--find-definitions-other-window :which-key "go to definition other")
   "xg" '(xref--find-definitions :which-key "go to definition")

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger

   ;; projectile
   "p" '(:ignore t :which-key "projectile")
   "pf" '(projectile-find-file :which-key "find file")
   "pr" '(projectile-recentf-files :which-key "recent files")
   "ps" '(projectile-ag :which-key "search")

   ;; Git
   "m" '(:ignore t :which-key "git")
   "ms" '(magit-status :which-key "git status")
   "ml" '(magit-log :which-key "git log")
   "md" '(magit-diff :which-key "git diff")
   "mc" '(magit-commit :which-key "git commit")
   "mf" '(magit-fetch :which-key "git fetch")

   ;; window
   "w" '(:ignore t :which-key "window")
   "wc" '(evil-window-delete :which-key "delete")
   "wo" '(delete-other-windows :which-key "delete other")
   "wb" '(balance-windows :which-key "balance")
   "wr" '(evil-window-rotate-downwards :which-key "rotate")
   "w," '(evil-window-decrease-height :which-key "decrease height")
   "w." '(evil-window-increase-height :which-key "increase height")
   )
  )

(provide 'init-general)
