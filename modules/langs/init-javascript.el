;;; init-javascript.el --- Initialize javascript configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "^node_modules$" "^flow-typed$"))


;;
;;; Major modes

(use-package rjsx-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :hook (rjsx-mode . rainbow-delimiters-mode)
  :init
  ;; Parse node stack traces in the compilation buffer
  (after! compilation
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                        2 3 4)))
  :config
  (setq js-chain-indent t
        ;; These have become standard in the JS community
        js2-basic-offset 2
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15)

  (setq-hook! 'rjsx-mode-hook
    ;; Indent switch-case another step
    js-switch-indent-offset js2-basic-offset)

  (use-package xref-js2
    :init
    (setq xref-js2-search-program 'rg))

  ;; HACK `rjsx-electric-gt' relies on js2's parser to tell it when the cursor
  ;;      is in a self-closing tag, so that it can insert a matching ending tag
  ;;      at point. The parser doesn't run immediately however, so a fast typist
  ;;      can outrun it, causing tags to stay unclosed, so force it to parse:
  (defadvice! +javascript-reparse-a (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    :before #'rjsx-electric-gt
    (if (= n 1) (rjsx-maybe-reparse))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  :init
  (autoload 'typescript-tsx-mode "typescript-mode" nil t)

  ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;;        `web-mode' because `typescript-mode' does not officially support
  ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'"
                     #'typescript-tsx-mode))

  (after! flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
    (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
  :config
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
    (after! lsp-mode
      (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))
    (after! evil-textobj-tree-sitter
      (pushnew! evil-textobj-tree-sitter-major-mode-language-alist '(typescript-tsx-mode . "tsx")))
    (after! tree-sitter
      (pushnew! tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))
    ;; HACK: the tsx grammer doesn't work with the hightlighting provided by
    ;;   font-lock-keywords. See emacs-tree-sitter/tree-sitter-langs#23
    (setq-hook! 'typescript-tsx-mode-hook
      tree-sitter-hl-use-font-lock-keywords nil))

  ;; HACK Fixes comment continuation on newline
  (autoload 'js2-line-break "js2-mode" nil t)
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break

    ;; Most projects use either eslint, prettier, .editorconfig, or tsf in order
    ;; to specify indent level and formatting. In the event that no
    ;; project-level config is specified (very rarely these days), the community
    ;; default is 2, not 4. However, respect what is in tsfmt.json if it is
    ;; present in the project
    typescript-indent-level
    (or (and (bound-and-true-p tide-mode)
             (plist-get (tide-tsfmt-options) :indentSize))
        typescript-indent-level)

    ;; Fix #5556: expand .x to className="x" instead of class="x", if
    ;; `emmet-mode' is used.
    emmet-expand-jsx-className? t))

;;
;;; Tools

(add-hook! '(js2-mode-hook
             typescript-mode-hook
             typescript-tsx-mode-hook
             rjsx-mode-hook)
           :append #'lsp!)

(add-hook! '(js2-mode-hook
             typescript-mode-hook
             typescript-tsx-mode-hook
             rjsx-mode-hook)
           :append #'tree-sitter!)

(use-package js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :init
  (map! :after js2-mode
        :map js2-mode-map
        :localleader
        (:prefix ("r" . "refactor")
          (:prefix ("a" . "add/arguments"))
          (:prefix ("b" . "barf"))
          (:prefix ("c" . "contract"))
          (:prefix ("d" . "debug"))
          (:prefix ("e" . "expand/extract"))
          (:prefix ("i" . "inject/inline/introduce"))
          (:prefix ("l" . "localize/log"))
          (:prefix ("o" . "organize"))
          (:prefix ("r" . "rename"))
          (:prefix ("s" . "slurp/split/string"))
          (:prefix ("t" . "toggle"))
          (:prefix ("u" . "unwrap"))
          (:prefix ("v" . "var"))
          (:prefix ("w" . "wrap"))
          (:prefix ("3" . "ternary"))))
  :config
  (add-hook 'js2-refactor-mode-hook #'evil-normalize-keymaps)
  (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
    (js2r-add-keybindings-with-prefix (format "%s r" my-localleader-key))))

(use-package npm-mode
  :hook ((js-mode typescript-mode) . npm-mode)
  :config
  (map! :localleader
        (:map npm-mode-keymap
          "n" npm-mode-command-keymap)
        (:after js2-mode
          :map js2-mode-map
          :prefix ("n" . "npm"))))


(provide 'init-javascript)
