;;; elisp.el -*- lexical-binding: t; -*-

(use-package elisp-mode
  :hook (emacs-lisp-mode . lsp!))

(provide 'init-elisp)
