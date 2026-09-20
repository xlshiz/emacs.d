;;; init-shell.el --- config shell. -*- lexical-binding: t; -*-
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
  ;; "M-o" to ace-window
  (defadvice! +evil-collection-vterm-setup-h ()
    :after #'evil-collection-vterm-setup
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "M-o") 'ace-window)
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-x C-f") 'find-file)
    ;; HACK!!
    (map! (:map vterm-mode-map
      :gi "C-u" #'vterm--self-insert))
    (evil-collection-define-key 'normal 'vterm-mode-map
      (kbd "M-o") 'ace-window))
  :init
  (setq vterm-always-compile-module nil)
  (setq vterm-buffer-name "vterm*")
  :config
  (setq vterm-keymap-exceptions (cl-delete "C-u" vterm-keymap-exceptions :test #'equal))
  (map! (:map vterm-mode-map
    :e "M-o" #'ace-window))
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it aroun
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)

  (setq-hook! 'vterm-mode-hook
              ;; Don't prompt about dying processes when killing vterm
              confirm-kill-processes nil
              show-trailing-whitespace nil
              ;; Prevent premature horizontal scrolling
              hscroll-margin 0)
  (add-to-list 'vterm-keymap-exceptions "C-j"))

(use-package ghostel
  :bind (:map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill))
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl")))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide 'init-shell)
