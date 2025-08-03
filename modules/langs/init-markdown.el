;;; init-markdown.el --- custom markdown mode. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package markdown-mode
  :mode (("\\.mmark\\'" . markdown-mode)
         ("README\\.md$'" . gfm-mode)
         ("\\.md$'" . markdon-mode)
         ("\\.markdown$'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  ;; Redefine the `auto-mode-alist' entries provided by
  ;; `markdown-mode', because `markdown-mode' adds them to the end of
  ;; the list, and in Emacs 26 an earlier entry takes precedence to
  ;; cause files named "CHANGELOG.md" to open in ChangeLog mode
  ;; instead of Markdown mode.
  ;; issue https://github.com/jrblevin/markdown-mode/issues/331
  (dolist (regex '("\\.md\\'" "\\.markdown\\'"))
    (setq auto-mode-alist
          (cl-remove regex auto-mode-alist :test #'equal :key #'car))
    (add-to-list 'auto-mode-alist `(,regex . markdown-mode)))
  :config
  ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
  (setq-hook! 'markdown-mode-hook
    fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                 fill-nobreak-predicate))
(map! :map markdown-mode-map
        :localleader
        "'" #'markdown-edit-code-block
        "o" #'markdown-open
        "P" #'grip-mode
        "e" #'markdown-export
        (:prefix ("i" . "insert")
         :desc "Table Of Content"  "T" #'markdown-toc-generate-toc
         :desc "Image"             "i" #'markdown-insert-image
         :desc "Link"              "l" #'markdown-insert-link
         :desc "<hr>"              "-" #'markdown-insert-hr
         :desc "Heading 1"         "1" #'markdown-insert-header-atx-1
         :desc "Heading 2"         "2" #'markdown-insert-header-atx-2
         :desc "Heading 3"         "3" #'markdown-insert-header-atx-3
         :desc "Heading 4"         "4" #'markdown-insert-header-atx-4
         :desc "Heading 5"         "5" #'markdown-insert-header-atx-5
         :desc "Heading 6"         "6" #'markdown-insert-header-atx-6
         :desc "Code block"        "C" #'markdown-insert-gfm-code-block
         :desc "Pre region"        "P" #'markdown-pre-region
         :desc "Blockquote region" "Q" #'markdown-blockquote-region
         :desc "Checkbox"          "[" #'markdown-insert-gfm-checkbox
         :desc "Bold"              "b" #'markdown-insert-bold
         :desc "Inline code"       "c" #'markdown-insert-code
         :desc "Italic"            "e" #'markdown-insert-italic
         :desc "Footnote"          "f" #'markdown-insert-footnote
         :desc "Header dwim"       "h" #'markdown-insert-header-dwim
         :desc "Italic"            "i" #'markdown-insert-italic
         :desc "Kbd"               "k" #'markdown-insert-kbd
         :desc "Pre"               "p" #'markdown-insert-pre
         :desc "New blockquote"    "q" #'markdown-insert-blockquote
         :desc "Strike through"    "s" #'markdown-insert-strike-through
         :desc "Table"             "t" #'markdown-insert-table
         :desc "Wiki link"         "w" #'markdown-insert-wiki-link)
        (:prefix ("t" . "toggle")
         :desc "Inline LaTeX"      "e" #'markdown-toggle-math
         :desc "Code highlights"   "f" #'markdown-toggle-fontify-code-blocks-natively
         :desc "Inline images"     "i" #'markdown-toggle-inline-images
         :desc "URL hiding"        "l" #'markdown-toggle-url-hiding
         :desc "Markup hiding"     "m" #'markdown-toggle-markup-hiding
         :desc "Wiki links"        "w" #'markdown-toggle-wiki-links
         :desc "GFM checkbox"      "x" #'markdown-toggle-gfm-checkbox)))

(provide 'init-markdown)
;;; init-markdown ends here
