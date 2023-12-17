;;; lsp.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun lsp! ()
  "Dispatch to call the currently used lsp client entrypoint"
  (interactive)
  (pcase my-lsp-backend
    ('lsp-bridge
     (lsp-bridge-mode))
    ('lsp-mode
     (lsp-deferred))))
