(use-package lsp-python-ms
  :defer t
  :custom
  (lsp-python-ms-dir (expand-file-name "~/.emacs.d/elisp/python-language-server/output/bin/Release/"))
  (lsp-python-ms-executable "~/.emacs.d/elisp/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(use-package python
  :defer t
  :delight "π "
  ;; TODO redefine using general
  ;; :bind (("M-[" . python-nav-backward-block)
  ;;        ("M-]" . python-nav-forward-block))
  )

(provide 'lang-py)
