;;; lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun lsp! ()
  "Dispatch to call the currently used lsp client entrypoint"
  (interactive)
  (pcase my-lsp-backend
    ('lsp-bridge
     (lsp-bridge-mode))
    ('lsp-mode
     (lsp-deferred))))
