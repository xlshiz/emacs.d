;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package lsp-bridge
  :defer t
  :init
  (add-hook! 'lsp-bridge-mode-hook #'evil-normalize-keymaps)
  (setq lsp-bridge-multi-lang-server-extension-list
      '(
        (("vue") . "volar_vtsls")
        (("ts" "mts")  . "typescript_eslint")
        (("tsx") . "typescriptreact_eslint")
        (("component.html") . "angular_template_html")
        (("component.ts") . "angular_template_typescript")
        ))
  :custom
  (lsp-bridge-user-langserver-dir (concat my-etc-dir "lsp-bridge/langserver"))
  (lsp-bridge-user-multiserver-dir (concat my-etc-dir "lsp-bridge/multiserver"))
  :config
  (setq lsp-bridge-enable-diagnostics t
        acm-enable-tabnine nil
        acm-enable-yas nil
        acm-backend-search-file-words-candidate-min-length 3
        lsp-bridge-enable-log nil)
  (map! (:map acm-mode-map
              [C-return]    (cmd! (acm-hide)
                                  (newline-and-indent))))
  (map! :map lsp-bridge-mode-map
        :n    "ga"  #'xref-find-apropos
        :n    "gd"  #'lsp-bridge-find-def
        :n    "gi"  #'lsp-bridge-find-impl
        :n    "gr"  #'lsp-bridge-find-references
        :n    "ge"  #'lsp-bridge-diagnostic-list
        :n    "gt"  #'lsp-bridge-find-type-def
        :n    "gj"  #'lsp-bridge-diagnostic-jump-next
        :n    "gk"  #'lsp-bridge-diagnostic-jump-prev
        :n    "K"   #'lsp-bridge-popup-documentation
        :n    "C-t" #'lsp-bridge-find-def-return
        :n    "gR"  #'lsp-bridge-rename))

(provide 'init-lsp)
;;; init-lsp.el ends here
