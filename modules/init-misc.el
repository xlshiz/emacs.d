;;; init-misc.el --- misc config files. -*- lexical-binding: t; -*-
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

;; Bookmark 设置
(use-package bookmark
  :defer t)

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :hook ((after-init . recentf-mode)
         (kill-emacs-hook . recentf-cleanup))
  :init
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 'never
        recentf-exclude '("/tmp/"
                          "/usr/local/Cellar/"
                          "recentf$"
                          "\\.cask$"
                          "\\.mkv$"
                          "\\.mp[34]$"
                          "\\.avi$"
                          "\\.wav$"
                          "\\.pdf$"
                          "\\.docx?$"
                          "\\.xlsx?$"
                          "url"
                          "COMMIT_EDITMSG\\'"
                          "bookmarks"
                          "pyim"
                          my-local-dir
                          my-cache-dir
                          my-etc-dir
                          (lambda (file) (file-in-directory-p file package-user-dir))
                          (lambda (file) (file-in-directory-p file my-local-dir))
                          (lambda (file) (file-in-directory-p file my-etc-dir))
                          (lambda (file) (file-in-directory-p file my-cache-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (setq recentf-filename-handlers
        (append '(abbreviate-file-name) recentf-filename-handlers)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; A better *Help* buffer.
(use-package helpful
  :commands helpful--buffer
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :hook (helpful-mode . cursor-sensor-mode)) ; for remove-advice button

(use-package restart-emacs
  :commands restart-emacs)

;; An alternative M-x interface for Emacs
(use-package amx
  :defer t
  :init (setq amx-history-length 10))

(use-package avy
  :defer t
  :init
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil
        avy-timeout-seconds 0.5))

(use-package color-rg
  :commands (color-rg-search-input color-rg-search-symbol
              color-rg-search-symbol-in-current-file color-rg-search-project)
  :init
  (defconst evil-collection-color-rg-maps '(color-rg-mode-map
                                             color-rg-mode-edit-map))
  :config
  (advice-add #'color-rg-update-header-line :override #'ignore)
  (defhydra color-rg-hydra (:hint nil)
    "
    ^^^^Move               ^^^^filter                     ^^toggle            ^^change
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
    _n_   next keyword   | _r_   replace all            | _I_  toggle ignore  | _d_  change dir
    _p_   prev keyword   | _f_   filter match result    | _c_  toggle case    | _z_  change globs
    _N_   next file      | _F_   filter mismatch result | _i_  open edit mode | _Z_  change exclude
    _P_   prev file      | _x_   filter match files     | ^^                  | _t_  return literal
    _D_   remove line    | _X_   filter mismatch files  | _u_  unfilter       | _s_  return regexp
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
  "
    ("n" color-rg-jump-next-keyword)
    ("p" color-rg-jump-prev-keyword)
    ("N" color-rg-jump-next-file)
    ("P" color-rg-jump-prev-file)

    ("r" color-rg-replace-all-matches)
    ("f" color-rg-filter-match-results)
    ("F" color-rg-filter-mismatch-results)
    ("x" color-rg-filter-match-files)
    ("X" color-rg-mismatch-files)
    ("u" color-rg-unfilter)
    ("D" color-rg-remove-line-from-results)

    ("I" color-rg-rerun-toggle-ignore)
    ("t" color-rg-rerun-literal)
    ("c" color-rg-rerun-toggle-case)
    ("s" color-rg-rerun-regexp)
    ("d" color-rg-rerun-change-dir)
    ("z" color-rg-rerun-change-globs)
    ("Z" color-rg-rerun-change-exclude-files)
    ("C" color-rg-customized-search)
    ("i" color-rg-switch-to-edit-mode)
    ("q" nil "quit"))
  (after! evil-collection
    (evil-collection-color-rg-setup)))

(provide 'init-misc)
;;; init-misc.el ends here
