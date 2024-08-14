;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Python Mode
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp!)
  :hook (python-mode . rainbow-delimiters-mode)
  :init
  (setq python-indent-offset 4
        python-fill-docstring-style 'django
        python-indent-guess-indent-offset nil
        python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (setq lsp-bridge-python-lsp-server "pyright")
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)
  ;; Env vars
  (after! exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(provide 'init-python)
;;; init-python.el ends here
