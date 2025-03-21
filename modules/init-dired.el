;;; init-dired.el --- config dired mode. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package dired
  :defer t
  :init
  (setq dired-dwim-target t            ; select another buffer as target when this is two dired buffer
        dired-isearch-filenames 'dwim  ; Search file name only when focus is over file
        dired-recursive-copies 'always ; always copy recursively
        dired-recursive-deletes 'top   ; always delete recursively
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil)
  :config
  (defun +dired/quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (mapc #'kill-buffer (my-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers"))
  (map! :map dired-mode-map
    :n "h" 'dired-up-directory
    :n "j" 'dired-next-line
    :n "k" 'dired-previous-line
    :n "l" 'dired-find-file
    :n "i" 'wdired-change-to-wdired-mode
    :n "th" 'dired-omit-mode
    :n "q" '+dired/quit-all

    :n "a" 'dired-find-alternate-file
    :n "d" 'dired-flag-file-deletion
    :n "gy" 'dired-show-file-type
    :n "gr" 'revert-buffer
    :n "ti" 'dired-toggle-read-only
    :n "m" 'dired-mark
    :n "o" 'dired-sort-toggle-or-edit
    :n "r" 'dired-do-redisplay
    :n "tt" 'dired-toggle-marks
    :n "u" 'dired-unmark
    :n "v" 'dired-git-info-mode
    :n "x" 'dired-do-flagged-delete
    :n "RET" 'dired-find-file
    ;; Commands to mark or flag certain categories of files
    :n "+" 'dired-create-directory
    :n "#" 'dired-flag-auto-save-files
    :n "." 'dired-clean-directory
    :n "~" 'dired-flag-backup-files
    :n "!" 'dired-do-shell-command
    :n "&" 'dired-do-async-shell-command
    ;; Upper case keys (except !) for operating on the marked files
    :n "A" 'dired-do-find-regexp
    :n "C" 'dired-do-copy
    :n "B" 'dired-do-byte-compile
    :n "D" 'dired-do-delete
    :n "G" 'dired-do-chgrp
    :n "H" 'dired-do-hardlink
    :n "I" 'dired-maybe-insert-subdir
    :n "J" 'dired-goto-file
    :n "K" 'dired-do-kill-lines
    :n "L" 'dired-do-load
    :n "M" 'dired-do-chmod
    :n "O" 'dired-do-chown
    :n "P" 'dired-do-print
    :n "Q" 'dired-do-find-regexp-and-replace
    :n "R" 'dired-do-rename
    :n "S" 'dired-do-symlink
    :n "T" 'dired-do-touch
    :n "W" 'browse-url-of-dired-file
    :n "X" 'dired-do-shell-command
    :n "Y" 'dired-copy-filename-as-kill
    :n "Z" 'dired-do-compress)
  (when is-mac-p
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls") ; brew install coreutils
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and is-mac-p (executable-find "gls"))
            (and (not is-mac-p) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  ;; Use single buffer
  (defadvice dired-find-file (around dired-find-file-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-file-for-visit)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer)))
      ad-do-it
      (kill-buffer orig)))

  ;; Colourful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package nerd-icons-dired
    :if (display-graphic-p)
    :diminish
    :hook (dired-mode . nerd-icons-dired-mode)
    :config
    (setq nerd-icons-dired-monochrome nil))

  (use-package dired-x
    :diminish dired-omit-mode
    :hook (dired-mode . dired-omit-mode)
    :config
    (setq dired-omit-verbose nil)
    (let ((cmd (cond
                (is-mac-p "open")
                (is-linux-p "xdg-open")
                (my/window-p "start")
                (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))
    ;; Don’t ask whether to kill buffers visiting deleted files
    (setq dired-clean-confirm-killing-deleted-buffers nil)
    (setq dired-omit-files (concat dired-omit-files
                                   "\\|^\\..*"
                                   "\\|^bazel*"))))

(use-package dirvish
  :defer t
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-quick-access-entries
    '(("d" "~/Downloads/"                "Downloads")
      ("j" "~/workdir/src/jd/"           "JD")
      ("h" "~/"                          "Home")))
  :init
  (dirvish-override-dired-mode)
  :config
  ;; (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see FAQ.org
  (setq dirvish-cache-dir (concat my-cache-dir "dirvish/")
        dirvish-hide-details '(dirvish-side)
        dirvish-attributes '(subtree-state collapse file-size file-time)
        dirvish-side-auto-expand nil
        dirvish-subtree-state-style 'nerd
        dirvish-reuse-session nil
        dirvish-use-mode-line nil
        dirvish-header-line-height '(1 . 1)
        delete-by-moving-to-trash nil
        dired-recursive-deletes 'always
        dired-dwim-target t
        dired-omit-files (concat "\\`[.]?#\\|\\`[.][.]?\\'"
                                 "\\|^\\..*"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dirvish-mode-line-format
    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (defadvice dirvish-find-entry-a (around dirvish-find-file-single-buffer activate)
    "Replace current buffer if file is a directory."
    (let ((orig (current-buffer))
          filename)
      (ignore-errors
        (setq filename (dired-get-file-for-visit)))
      ad-do-it
      (when (and filename
                 (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (map! :map dired-mode-map
    :ng "h"   #'dired-up-directory
    :ng "j"   #'dired-next-line
    :ng "k"   #'dired-previous-line
    :ng "l"   #'dired-find-file
    :ng "i"   #'wdired-change-to-wdired-mode
    :ng "."   #'dired-omit-mode
    :ng "TAB" #'dirvish-subtree-toggle
    :ng "M-n" #'dirvish-history-go-forward
    :ng "M-p" #'dirvish-history-go-backward
    :ng "M-s" #'dirvish-setup-menu
    :ng "M-f" #'dirvish-toggle-fullscreen
    :ng "*"   #'dirvish-mark-menu
    :ng "r"   #'dirvish-fd-roam
    :ng "b"   #'dirvish-quick-access
    :ng "z"   #'dirvish-show-history
    :ng "f"   #'dirvish-file-info-menu
    :ng [remap dired-sort-toggle-or-edit] #'dirvish-quicksort
    :ng [remap dired-do-redisplay] #'dirvish-ls-switches-menu
    :ng [remap dired-summary] #'dirvish-dispatch
    :ng [remap dired-do-copy] #'dirvish-yank-menu
    :ng [remap mode-line-other-buffer] #'dirvish-history-last)
  (global-set-key [remap find-dired] #'dirvish-fd))

(provide 'init-dired)
;;; init-dired ends here
