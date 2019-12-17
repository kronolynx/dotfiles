(use-package org
  :requires org-plus-contrib
  :preface
  (defun my/org-compare-times (clocked estimated)
    "Gets the ratio between the timed time and the estimated time."
    (if (and (> (length clocked) 0) estimated)
        (format "%.2f"
                (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                   (org-hh:mm-string-to-minutes estimated)))
      ""))

  (defun my/org-archive-done-tasks ()
    "Archives finished or cancelled tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

  (defun my/org-jump ()
    "Jumps to a specific task."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-refile)))

  (defun my/org-use-speed-commands-for-headings-and-lists ()
    "Activates speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
        (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
  (defmacro ignore-args (fnc)
    "Returns function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-directory "~/Dropbox/org")
  (org-default-notes-file (concat org-directory "/todo.org"))
  (org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
  (org-mobile-directory "~/Dropbox/MobileOrg")
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . t)))
  (org-cycle-include-plain-lists 'integrate)
  (org-expiry-inactive-timestamps t)
  (org-export-backends '(ascii beamer html icalendar latex man md org texinfo))
  (org-log-done 'time)
  (org-log-into-drawer "LOGBOOK")
  (org-modules '(org-crypt
                 org-habit
                 org-info
                 org-irc
                 org-mouse
                 org-protocol
                 org-tempo))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-cache nil)
  (org-refile-use-outline-path nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-startup-folded nil)
  (org-startup-with-inline-images t)
  (org-tag-alist '(("@coding" . ?c)
                   ("@nix" . ?u)
                   ("@errands" . ?e)
                   ("@home" . ?h)
                   ("@phone" . ?p)
                   ("@reading" . ?r)
                   ("@learning" . ?l)
                   ("@work" . ?w)
                   ("fuzzy" . ?0)
                   ("highenergy" . ?1)))
  (org-tags-exclude-from-inheritance '("project"))
  (org-todo-keywords '((sequence "TODO(t)"
                                 "STARTED(s)"
                                 "WAITING(w@/!)"
                                 "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
                       (sequence "TOBUY"
                                 "TOSHRINK"
                                 "TOCUT"
                                 "TOSEW" "|" "DONE(x)")))
  (org-use-effective-time t)
  (org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)
  (org-yank-adjusted-subtrees t)
  :config
  (add-to-list 'org-global-properties '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (advice-add 'org-deadline :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-schedule :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-todo :after (ignore-args #'org-save-all-org-buffers))
  (org-clock-persistence-insinuate)
  (org-load-modules-maybe t))

(use-package toc-org ;; :TOC: tag for table of contents
  :after org
  :hook (org-mode . toc-org-enable))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "►" "▸")))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))


(provide 'init-org)
