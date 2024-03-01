;;; init-cc.el --- c/cpp config. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package cc-mode
  :mode ("\\.h|\\.cpp" . c++-mode)
  :hook ((c-mode c++-mode) . my/cxx-mode-setup)
  :bind (:map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :init
  (setq cc-other-file-alist
        '(("\\.c\\'"   (".h"))
          ("\\.C\\'"   (".h" ".hpp" ".hxx"))
          ("\\.cc\\'"  (".h" ".hpp" ".hxx"))
          ("\\.cpp\\'" (".h" ".hpp" ".hxx"))
          ("\\.cxx\\'" (".h" ".hpp" ".hxx"))
          ("\\.tpp\\'" (".h" ".hpp" ".hxx"))
          ("\\.tcc\\'" (".h" ".hpp" ".hxx"))
          ("\\.h\\'"   (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".hxx" ".hpp"))
          ("\\.hpp\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))
          ("\\.hxx\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))))
  (defun my/cxx-mode-setup ()
    (use-package modern-cpp-font-lock
      :diminish modern-c++-font-lock-mode
      :config
      (modern-c++-font-lock-global-mode t))

    (setq show-trailing-whitespace t)
    ;; used by ff-find-other-file
    (setq cc-search-directories '("."
                                  "../include"
                                  "../*/include"
                                  "/usr/include"
                                  "/usr/local/include/*"
                                  "../src"
                                  "../src/*"
                                  "../../src/*"
                                  "../../../src/*"
                                  "../../src/*/*"
                                  "../../../src/*/*/*")))
  :config
  (when (eq my-lsp-backend 'lsp-mode)
    (setq lsp-clients-clangd-args '("-j=3"
                                    "--background-index"
                                    "--clang-tidy"
                                    "--completion-style=detailed"
                                    "--header-insertion=never"
                                    "--header-insertion-decorators=0")))

  (set-formatter!
    'clang-format
    '("clang-format"
      "-assume-filename"
      (or (buffer-file-name)
          (cdr (assoc major-mode
                      '((c-mode        . ".c")
                        (c++-mode      . ".cpp")
                        (cuda-mode     . ".cu")
                        (protobuf-mode . ".proto"))))))
    :modes '(c-mode c++-mode protobuf-mode cuda-mode))

  (setf (alist-get 'other c-default-style) "my-c-style")
  (c-add-style
   "my-c-style" '("linux"
            (c-comment-only-line-offset . 0)
            (tab-width . 8)
            (c-basic-offset . 8)
            (indent-tabs-mode . t)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (label . 0))))

  (setq c-basic-offset 8)
  (add-hook! '(c-mode-hook
               c++-mode-hook) :append #'tree-sitter!))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4)
  ;; (add-to-list 'company-backends 'company-cmake)

  (use-package cmake-font-lock
    :config
    (add-hook 'cmake-mode-hook 'font-lock-refresh-defaults)))

(after! consult-imenu
        (add-to-list 'consult-imenu-config '(c-mode :toplevel "function"
                                                    :types ((?f "function" font-lock-function-name-face)
                                                            (?m "macro"    font-lock-function-name-face)
                                                            (?p "prototype"  font-lock-constant-face)
                                                            (?v "variable" font-lock-variable-name-face)
                                                            (?t "typedef"      font-lock-type-face)))))

(provide 'init-cc)
;;; init-cxx.el ends here
