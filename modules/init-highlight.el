;; init-highlight.el --- Initialize highlight configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Highlight the current line
(use-package hl-line
  :hook (((evil-visual-state-entry activate-mark) . my/disable-hl-line)
         ((evil-visual-state-exit deactivate-mark) . my/enable-hl-line)
         ((prog-mode text-mode conf-mode special-mode) . hl-line-mode))
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  ;; Temporarily disable `hl-line' when selection is active
  (defvar my/-hl-line-mode nil)
  (defun my/disable-hl-line ()
    (when hl-line-mode
      (hl-line-mode -1)
      (setq-local my/-hl-line-mode t)))
  (defun my/enable-hl-line ()
    (when my/-hl-line-mode
      (hl-line-mode +1))))

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (set-face-foreground 'show-paren-match "red")      ;定义前景色
  (set-face-bold 'show-paren-match t)                ;加粗显示括号匹配
  (set-face-background 'show-paren-match nil)        ;定义背景色
  (set-face-underline 'show-paren-match t)           ;显示下划线
  (setq show-paren-delay 0.1
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight number.
(use-package highlight-numbers
  :if (display-graphic-p)
  :hook ((prog-mode conf-mode protobuf-mode yaml-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :defer t
  :config
  (setq rainbow-delimiters-max-face-count 3))

;; Highlight TODO/FIXME/NOTE...
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("BUG" error bold)
                                ("FIXME" error bold)
                                ("TODO" warning bold)
                                ("NOTE" success bold)
                                ("HACK" font-lock-constant-face bold)
                                ("REVIEW" font-lock-keyword-face bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

;; Pulse current line
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my/recenter-and-pulse-line))
  :init
  (defun my/pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my/pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my/pulse-momentary-line)))

  (defun my/recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (my/pulse-momentary))

  (defun my/recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my/pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 ace-window
                 winum-select-window-1
                 winum-select-window-2
                 winum-select-window-3
                 winum-select-window-4
                 winum-select-window-5
                 winum-select-window-6
                 winum-select-window-7
                 winum-select-window-8
                 winum-select-window-9
                 winum-select-window-0-or-10
                 pager-page-down pager-page-up
                 lsp-bridge-ref-jump-next-keyword lsp-bridge-ref-jump-prev-keyword
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my/pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 neotree-enter
                 xref-go-back xref-go-forward
                 evil-jump-backward evil-jump-forward
                 better-jumper-jump-forward better-jumper-jump-backward
                 diff-hl-previous-hunk diff-hl-next-hunk
                 goto-last-change))
    (advice-add cmd :after #'my/recenter-and-pulse)))

;; Highlight symbols, copy from centaur emacs
(use-package symbol-overlay
  :diminish
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :init
  (setq symbol-overlay-idle-time 0.1)
  (setq symbol-overlay-inhibit-map t)
  (after! nerd-icons
    (setq symbol-overlay-faces
          '((:inherit (nerd-icons-blue bold) :inverse-video t)
            (:inherit (nerd-icons-pink bold) :inverse-video t)
            (:inherit (nerd-icons-yellow bold) :inverse-video t)
            (:inherit (nerd-icons-purple bold) :inverse-video t)
            (:inherit (nerd-icons-red bold) :inverse-video t)
            (:inherit (nerd-icons-orange bold) :inverse-video t)
            (:inherit (nerd-icons-green bold) :inverse-video t)
            (:inherit (nerd-icons-cyan bold) :inverse-video t)))))

(provide 'init-highlight)
;;; init-highlight.el ends here
