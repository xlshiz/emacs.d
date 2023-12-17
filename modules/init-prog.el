;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:
;;; Code:

(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook #'xref-after-jump-hook #'recenter)
  (add-hook #'xref-after-jump-hook #'better-jumper-set-jump)
  (add-hook #'xref-after-return-hook #'recenter)
  (add-hook #'xref-after-return-hook #'better-jumper-set-jump))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))

(use-package tree-sitter
  :defer t
  :custom-face
  (tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face))))
  :init
  (defvar +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode)
    "A list of major modes which should be highlighted by tree-sitter.

    If this list begins with `not', then it negates the list.
    If it is t, it is enabled in all modes.
    If nil, it is disabled in all modes")
  :config
  (require 'tree-sitter-langs)
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t))

(use-package evil-textobj-tree-sitter
  :defer t
  :init (after! tree-sitter (require 'evil-textobj-tree-sitter))
  :config
  (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

  (general-evil-define-key '(visual operator) global
    "i" +tree-sitter-inner-text-objects-map
    "a" +tree-sitter-outer-text-objects-map)
  (general-nmap
    "[g" +tree-sitter-goto-previous-map
    "]g" +tree-sitter-goto-next-map)

  (general-define-key :keymaps '+tree-sitter-inner-text-objects-map
         "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
         "f" (+tree-sitter-get-textobj "function.inner")
         "F" (+tree-sitter-get-textobj "call.inner")
         "C" (+tree-sitter-get-textobj "class.inner")
         "v" (+tree-sitter-get-textobj "conditional.inner")
         "l" (+tree-sitter-get-textobj "loop.inner"))
  (general-define-key :keymaps '+tree-sitter-outer-text-objects-map
         "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
         "f" (+tree-sitter-get-textobj "function.outer")
         "F" (+tree-sitter-get-textobj "call.outer")
         "C" (+tree-sitter-get-textobj "class.outer")
         "c" (+tree-sitter-get-textobj "comment.outer")
         "v" (+tree-sitter-get-textobj "conditional.outer")
         "l" (+tree-sitter-get-textobj "loop.outer"))

  (general-define-key :keymaps '+tree-sitter-goto-previous-map
         "a" (+tree-sitter-goto-textobj "parameter.outer" t)
         "f" (+tree-sitter-goto-textobj "function.outer" t)
         "F" (+tree-sitter-goto-textobj "call.outer" t)
         "C" (+tree-sitter-goto-textobj "class.outer" t)
         "c" (+tree-sitter-goto-textobj "comment.outer" t)
         "v" (+tree-sitter-goto-textobj "conditional.outer" t)
         "l" (+tree-sitter-goto-textobj "loop.outer" t))
  (general-define-key :keymaps '+tree-sitter-goto-next-map
         "a" (+tree-sitter-goto-textobj "parameter.outer")
         "f" (+tree-sitter-goto-textobj "function.outer")
         "F" (+tree-sitter-goto-textobj "call.outer")
         "C" (+tree-sitter-goto-textobj "class.outer")
         "c" (+tree-sitter-goto-textobj "comment.outer")
         "v" (+tree-sitter-goto-textobj "conditional.outer")
         "l" (+tree-sitter-goto-textobj "loop.outer")))


(use-package citre
  :init
  (setq
   citre-project-root-function #'projectile-project-root
   citre-default-create-tags-file-location 'project-cache
   citre-use-project-root-when-creating-tags nil
   ;; citre-peek-fill-fringe nil
   citre-prompt-language-for-ctags-command t)
  :config
  (require 'citre-config)
  ;; HACK!!
  ;; (add-hook 'citre-peek--mode-hook #'evil-normalize-keymaps)
  (general-nmap
         "g C-]"  #'citre-peek
         "C-]"    #'citre-jump
         "M-,"    #'citre-jump-back)
  (map! (:map citre-peek-keymap
         "M-n"     #'citre-peek-next-line
         "M-p"     #'citre-peek-prev-line
         :n [right]   #'citre-peek-chain-forward
         :n [left]    #'citre-peek-chain-backward
         "M-m p"   #'citre-peek-through)))
  ;; (general-evil-define-key '(normal motion) citre-peek-keymap
  ;;        "M-n"     #'citre-peek-next-line
  ;;        "M-p"     #'citre-peek-prev-line
  ;;        "M-N"     #'citre-peek-next-definition
  ;;        "M-P"     #'citre-peek-prev-definition
  ;;        [right]   #'citre-peek-chain-forward
  ;;        [left]    #'citre-peek-chain-backward
  ;;        [up]      #'citre-peek-prev-branch
  ;;        [down]    #'citre-peek-next-branch
  ;;        "M-m p"   #'citre-peek-through
  ;;        "M-m d"   #'citre-peek-delete-branch
  ;;        "M-m D"   #'citre-peek-delete-branches
  ;;        [S-up]    #'citre-peek-move-current-def-up
  ;;        [S-down]  #'citre-peek-move-current-def-down
  ;;        "M-m f"   #'citre-peek-make-current-def-first
  ;;        "M-m j"   #'citre-peek-jump
  ;;        "q"       #'citre-peek-abort))

(use-package symbols-outline
  :commands (symbols-outline-smart-toggle)
  :init
  (setq symbols-outline-follow-symbol-in-origin nil)
  :config
  ;; HACK!!
  (evil-make-overriding-map symbols-outline-mode-map 'normal)
  (general-define-key :keymaps 'symbols-outline-mode-map
         "g"       #'symbols-outline-refresh
         "j"       #'symbols-outline-next
         "k"       #'symbols-outline-prev
         "TAB"     #'symbols-outline-toggle-node
         "RET"     #'symbols-outline-visit
         "M-RET"   #'symbols-outline-visit-and-quit
         "q"       #'quit-window)
  (setq symbols-outline-window-position 'right)
  (symbols-outline-follow-mode))

(provide 'init-prog)
;;; init-prog.el ends here
