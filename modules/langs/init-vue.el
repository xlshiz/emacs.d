;;; init-vue.el --- Initialize vue configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(delete '("\\.vue\\'" . web-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(after! lsp-mode
  (setq lsp-vetur-validation-template nil))
(add-hook! 'vue-mode-hook #'lsp!)

(provide 'init-vue)
