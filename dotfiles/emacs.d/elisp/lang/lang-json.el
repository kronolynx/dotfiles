(use-package json-mode                  ; JSON files
  :defer t
  :config
  (progn                                ; https://github.com/skeeto/.emacs.d
    (setf json-reformat:pretty-string? t
          json-reformat:indent-width 2)
    ;; TODO redefine using general
    (define-key json-mode-map (kbd "M-q")
      (lambda ()
        (interactive)
        (if (region-active-p)
            (call-interactively #'json-reformat-region)
          (json-reformat-region (point-min) (point-max)))))

    (add-hook 'json-mode-hook
              ;; Fix JSON mode indentation
              (lambda () (setq-local js-indent-level 4)))))

(use-package json-reformat              ; Reformat JSON
  :defer t
  ;; TODO redefine using general
  ;; :bind (("C-c x j" . json-reformat-region))
  )

(provide 'lang-json)
