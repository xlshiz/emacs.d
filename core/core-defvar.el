;;; core-const.el --- const variable. -*- lexical-binding: t -*-
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

(defconst is-windows-p
  (eq system-type 'windows-nt)
  "Are we running on a Win system?")

(defconst is-linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst is-mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst is-emacs28-p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst my-local-dir (concat user-emacs-directory ".local/")
  "Emacs local directory.")
(unless (file-exists-p my-local-dir)
  (make-directory my-local-dir))

(defconst my-cache-dir (concat my-local-dir "cache/")
  "Emacs cache directory.")
(unless (file-exists-p my-cache-dir)
  (make-directory my-cache-dir))

(defconst my-etc-dir (concat my-local-dir "etc/")
  "Emacs cache directory.")
(unless (file-exists-p my-etc-dir)
  (make-directory my-etc-dir))

(defconst my-autoload-file (concat my-local-dir "autoloads.el")
  "This file is responsible for informing emacs where to find all autoload function in core/autoload/*.el")

(defconst my-http-proxy "127.0.0.1:8118"
  "Set http/https proxy.")

(defvar my-lsp-backend 'lsp-bridge
  "Which language server to use, eglot, lsp-mode or lsp-bridge")

(defvar my-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable. On some distros it's 'fdfind' (ubuntu,
debian, and derivatives). On most it's 'fd'.")

(defvar my-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar my-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar my-localleader-key ","
  "The localleader prefix key, for major-mode specific commands.")

(defvar my-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")


(provide 'core-defvar)
;;; core-const.el ends here
