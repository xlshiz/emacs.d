;;; init-shell.el --- config shell. -*- lexical-binding: t; -*-
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

(use-package vterm
  :commands vterm-mode
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  ;; "C-j" to ace-window
  (defadvice evil-collection-vterm-setup (after +evil-collection-vterm-setup-h activate)
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-j") 'ace-window)
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-x C-f") 'find-file)
    ;; HACK!!
    (map! (:map vterm-mode-map
      :gi "C-u" #'vterm--self-insert))
    (evil-collection-define-key 'normal 'vterm-mode-map
      (kbd "C-j") 'ace-window))
  :init
  (setq vterm-always-compile-module t)
  (setq vterm-buffer-name "vterm*")
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil)
                               (advice-add #'vterm--redraw :after (lambda (&rest args)
                                                                    (evil-refresh-cursor evil-state)))))
  :config
  (setq vterm-keymap-exceptions (cl-delete "C-u" vterm-keymap-exceptions :test #'equal))
  (add-to-list 'vterm-keymap-exceptions "C-j"))

(use-package vterm-toggle
  :defer t)

(provide 'init-shell)
