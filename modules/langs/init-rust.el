;; init-rust.el --- Initialize Rust configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp!)
  :hook (rust-mode . rainbow-delimiters-mode)
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :hook (flycheck-mode . flycheck-rust-setup)))

(provide 'init-rust)
;; init-rust.el ends here
