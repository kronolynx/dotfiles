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
  (general-def 'normal 'neotree-mode-map
    "z"   'neotree-toggle
    "j"   'neotree-next-line
    "k"   'neotree-previous-line
    "RET" 'neotree-enter
    "n"   'neotree-create-node
    "r"   'neotree-rename-node
    "d"   'neotree-delete-node
    "m"   'make-directory
    "SPC" 'neotree-change-root
    "q"   'neotree-hide
    "l"   'neotree-enter
    "g"   'neotree-refresh
    "A"   'neotree-stretch-toggle
    "H"   'neotree-hidden-file-toggle
    )
  (general-define-key
   :states '(scala-mode)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "xb" '(lsp-metals-build-connect :which-key "metals build")
   "xi" '(lsp-metals-build-import :which-key "metals import")
   "gi" '(sbt-find-definitions :which-key "implementation")
   "su" '(sbt-find-usages :which-key "usages")
   )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"


   ;; simple command
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "prev buffer")
   "SPC" '(avy-goto-word-or-subword-1  :which-key "jump to word")

   ;; buffers
   "t" '(:ignore t :which-key "buffer")
   "tl" '(counsel-buffer-or-recentf :which-key "switch or recent")
   "to" '(counsel-switch-buffer-other-window :which-key "other window")

   ;; files
   "f" '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "find")
   "fr" '(counsel-recentf :which-key "recent")
   "fg" '(counsel-git :which-key "find file in git dir")

   ;; search
   "s" '(:ignore t :which-key "search")
   "ss" '(counsel-grep-or-swiper :which-key "swiper")
   "sp" '(swiper-thing-at-point :which-key "swiper at point")
   "se" '(counsel-ag :which-key "everywhere")
   "sr" '(counsel-rg :which-key "rip-grep")
   "sg" '(counsel-git :which-key "git")

   ;; editor
   "e" '(:ignore t :which-key "editor")
   "ed" '(counsel-describe-function :which-key "describe function")
   "eb" '(evil-jump-backward :which-key "jump backward")
   "ef" '(evil-jump-forward :which-key "jump forward")


   ;; goto
   "g" '(:ignore t :which-key "goto")
   "gd" '(xref--find-definitions :which-key "go to definition")
   "go" '(xref--find-definitions-other-window :which-key "go to definition other")

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger

   ;; project
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
   "wg" '(balance-windows :which-key "balance")
   "wr" '(evil-window-rotate-downwards :which-key "rotate")
   "w>" '(evil-window-decrease-height :which-key "decrease height")
   "w<" '(evil-window-increase-height :which-key "increase height")
   "w," '(evil-window-decrease-width :which-key "decrease width")
   "w." '(evil-window-increase-width :which-key "increase width")
   "wv" '(evil-window-vsplit :which-key "vertical split")
   "ws" '(evil-window-split :which-key "horizontal split")
   "wm" '(:ignore t :which-key "move")
   "wml" '(evil-window-move-far-right :which-key "right")
   "wmh" '(evil-window-move-far-left :which-key "left")
   "wmt" '(evil-window-move-very-top :which-key "top")
   "wmb" '(evil-window-move-very-bottom :which-key "bottom")
   "wl" '(evil-window-next :which-key "right")
   "wh" '(evil-window-prev :which-key "left")
   "wk" '(evil-window-up :which-key "top")
   "wj" '(evil-window-down :which-key "bottom")

   ;; help
   "h" '(:ignore t :which-key "help")
   "hk" '(key-description :which-key "describe key")
   "hf" '(describe-function :which-key "describe function")
   "hb" '(describe-bindings :which-key "describe bindings")

   ;; misc
   "x" '(:ignore t :which-key "misc")
   "xu" '(counsel-unicode-char :which-key "unicode char")
   )
  )

(provide 'init-general)
