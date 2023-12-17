;;; init-edit.el --- insert description here -*- lexical-binding: t -*-
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
;;; Code:

;; 自动刷新文件
(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(use-package undo-fu
  :defer t
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))


(use-package undo-fu-session
  :hook (my-after-init . undo-fu-session-global-mode)
  :custom (undo-fu-session-directory (concat my-cache-dir "undo-fu-session/"))
  :config
  (setq undo-fu-session-file-limit 1000)
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  ;; HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
  ;;      filenames to prevent file paths that are too long, so we force
  ;;      `undo-fu-session--make-file-name' to use it instead of its own
  ;;      home-grown overly-long-filename generator.
  ;; TODO PR this upstream; should be a universal issue
  (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
    :override #'undo-fu-session--make-file-name
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file))
            (undo-fu-session--file-name-ext))))


(use-package vundo
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  (define-key vundo-mode-map [escape] #'vundo-quit))


;; smartparens
(use-package smartparens
  :hook (after-init . smartparens-global-mode)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil)
    ;; Smartparens conditional binds a key to C-g when sp overlays are active
    ;; (even if they're invisible). This disruptively changes the behavior of
    ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
    ;; I don't see the point of this keybind, so...
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'eval-expression-minibuffer-setup-hook
    (defun my-init-smartparens-in-eval-expression-h ()
      "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
      (when smartparens-global-mode (smartparens-mode +1))))
  (add-hook! 'minibuffer-setup-hook
    (defun my-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens' for non-`eval-expression' commands.
Only enable `smartparens-mode' if `smartparens-global-mode' is
on."
      (when (and smartparens-global-mode (memq this-command '(evil-ex)))
        (smartparens-mode +1))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar my-buffer-smartparens-mode nil)
  (add-hook! 'evil-replace-state-exit-hook
    (defun my-enable-smartparens-mode-maybe-h ()
      (when my-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'my-buffer-smartparens-mode))))
  (add-hook! 'evil-replace-state-entry-hook
    (defun my-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local my-buffer-smartparens-mode t)
        (turn-off-smartparens-mode)))))

(use-package yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  :config
  (setq yas-inhibit-overlay-modification-protection t)
  (advice-add 'yas--on-protection-overlay-modification :override #'ignore)
  (yas-global-mode +1))

(use-package auto-yasnippet
  :defer t
  :config
  (setq aya-persist-snippets-dir (expand-file-name "snippets/" user-emacs-directory))
  (defadvice! +snippets--inhibit-yas-global-mode-a (fn &rest args)
    "auto-yasnippet enables `yas-global-mode'. This is obnoxious for folks like
us who use yas-minor-mode and enable yasnippet more selectively. This advice
swaps `yas-global-mode' with `yas-minor-mode'."
    :around '(aya-expand aya-open-line)
    (letf! ((#'yas-global-mode #'yas-minor-mode)
            (yas-global-mode yas-minor-mode))
      (apply fn args))))

(use-package thing-edit
  :commands (thing-cut-parentheses thing-copy-parentheses thing-replace-parentheses
             thing-copy-region-or-line thing-cut-region-or-line thing-replace-region-or-line)
  :defer t)

(use-package avy-thing-edit
  :defer t)

(provide 'init-edit)
;;; init-edit.el ends here
