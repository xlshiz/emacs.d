;; init-web.el --- Initialize web configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar +web-continue-block-comments t
  "If non-nil, newlines in block comments are continued with a leading *.

This also indirectly means the asterisks in the opening /* and closing */ will
be aligned.

If set to `nil', disable all the above behaviors.")


(use-package emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
  (map! :map emmet-mode-keymap
        :v [tab] #'emmet-wrap-with-markup
        "M-E" #'emmet-expand-line))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :init
  ;; If the user has installed `vue-mode' then, by appending this to
  ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; priority over this one.
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  :mode "\\.vue\\'"
  :config

  ;; tidy is already defined by the format-all package. We redefine it to add
  ;; more sensible arguments to the tidy command.
  ;; (set-formatter! 'html-tidy
  ;;   '("tidy" "-q" "-indent"
  ;;     "--tidy-mark" "no"
  ;;     "--drop-empty-elements" "no"
  ;;     ("--show-body-only" "%s" (if +format-region-p "true" "auto"))
  ;;     ("--indent-spaces" "%d" tab-width)
  ;;     ("--indent-with-tabs" "%s" (if indent-tabs-mode "yes" "no"))
  ;;     ("-xml" (memq major-mode '(nxml-mode xml-mode))))
  ;;   :ok-statuses '(0 1))

  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)

  (after! smartparens
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (delq nil web-mode-engines-auto-pairs))

  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'"))

  ;; Use // instead of /* as the default comment delimited in JS
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal)
        "//")

  (add-hook! 'web-mode-hook
    (defun +web--fix-js-comments-h ()
      "Fix comment handling in `web-mode' for JavaScript."
      (when (member web-mode-content-type '("javascript" "jsx"))
        ;; For some reason the default is to insert HTML comments even
        ;; in JavaScript.
        (setq-local comment-start "//")
        (setq-local comment-end "")
        ;; Needed since otherwise the default value generated by
        ;; `comment-normalize-vars' will key off the syntax and think
        ;; that a single "/" starts a comment, which completely borks
        ;; auto-fill.
        (setq-local comment-start-skip "// *"))))

  (add-hook! 'web-mode-hook
    (defun +web--fix-indent-h ()
      (setq-local standard-indent 2)
      (setq-local web-mode-code-indent-offset standard-indent)
      (setq-local web-mode-markup-indent-offset standard-indent)
      (setq-local web-mode-css-indent-offset standard-indent)))

  (map! :map web-mode-map
        (:localleader
         :desc "Rehighlight buffer" "h" #'web-mode-reload
         :desc "Indent buffer"      "i" #'web-mode-buffer-indent
         (:prefix ("a" . "attribute")
                  "b" #'web-mode-attribute-beginning
                  "e" #'web-mode-attribute-end
                  "i" #'web-mode-attribute-insert
                  "n" #'web-mode-attribute-next
                  "s" #'web-mode-attribute-select
                  "k" #'web-mode-attribute-kill
                  "p" #'web-mode-attribute-previous
                  "t" #'web-mode-attribute-transpose)
         (:prefix ("b" . "block")
                  "b" #'web-mode-block-beginning
                  "c" #'web-mode-block-close
                  "e" #'web-mode-block-end
                  "k" #'web-mode-block-kill
                  "n" #'web-mode-block-next
                  "p" #'web-mode-block-previous
                  "s" #'web-mode-block-select)
         (:prefix ("d" . "dom")
                  "a" #'web-mode-dom-apostrophes-replace
                  "d" #'web-mode-dom-errors-show
                  "e" #'web-mode-dom-entities-encode
                  "n" #'web-mode-dom-normalize
                  "q" #'web-mode-dom-quotes-replace
                  "t" #'web-mode-dom-traverse
                  "x" #'web-mode-dom-xpath)
         (:prefix ("e" . "element")
                  "/" #'web-mode-element-close
                  "a" #'web-mode-element-content-select
                  "b" #'web-mode-element-beginning
                  "c" #'web-mode-element-clone
                  "d" #'web-mode-element-child
                  "e" #'web-mode-element-end
                  "f" #'web-mode-element-children-fold-or-unfold
                  "i" #'web-mode-element-insert
                  "k" #'web-mode-element-kill
                  "m" #'web-mode-element-mute-blanks
                  "n" #'web-mode-element-next
                  "p" #'web-mode-element-previous
                  "r" #'web-mode-element-rename
                  "s" #'web-mode-element-select
                  "t" #'web-mode-element-transpose
                  "u" #'web-mode-element-parent
                  "v" #'web-mode-element-vanish
                  "w" #'web-mode-element-wrap)
         (:prefix ("t" . "tag")
                  "a" #'web-mode-tag-attributes-sort
                  "b" #'web-mode-tag-beginning
                  "e" #'web-mode-tag-end
                  "m" #'web-mode-tag-match
                  "n" #'web-mode-tag-next
                  "p" #'web-mode-tag-previous
                  "s" #'web-mode-tag-select))

        :i  "SPC" #'self-insert-command
        :n  "za"  #'web-mode-fold-or-unfold
        :nv "]a"  #'web-mode-attribute-next
        :nv "[a"  #'web-mode-attribute-previous
        :nv "]t"  #'web-mode-tag-next
        :nv "[t"  #'web-mode-tag-previous
        :nv "]T"  #'web-mode-element-child
        :nv "[T"  #'web-mode-element-parent))

;; built-in. Contains both css-mode & scss-mode
(use-package css-mode
  :config
  (add-hook! 'css-mode-hook
    (defun +css--fix-comment-h ()
      ;; Correctly continue /* and // comments on newline-and-indent
      comment-line-break-function #'+css/comment-indent-new-line
      ;; Fix `fill-paragraph' not conjoining line comments in CSS modes correctly.
      adaptive-fill-function #'+css-adaptive-fill-fn
      ;; Fix filled lines not being auto-prefixed with a * when needed.
      adaptive-fill-first-line-regexp "\\'[ \t]*\\(?:\\* *\\)?\\'"))

  ;; css-mode hooks apply to scss and less-css modes
  (map! :localleader
        :map scss-mode-map
        "b" #'+css/scss-build
        :map (css-mode-map scss-mode-map less-css-mode-map)
        "rb" #'+css/toggle-inline-or-block))

(add-hook! '(css-mode-hook sass-mode-hook stylus-mode-hook)
           #'rainbow-mode)

(add-hook! '(html-mode-hook
             web-mode-hook
             nxml-mode-hook)
           :append #'lsp!)

(add-hook! '(html-mode-hook
             mhtml-mode-hook)
           :append #'tree-sitter!)

(add-hook! '(css-mode-hook
             scss-mode-hook
             sass-mode-hook
             less-css-mode-hook)
           :append #'lsp!)

(add-hook 'css-mode-hook #'tree-sitter! 'append)

(provide 'init-web)
;;; init-web ends here
