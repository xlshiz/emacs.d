;;; edit/restart.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar desktop-base-file-name)
(defvar desktop-dirname)
(defvar desktop-restore-eager)
(defvar desktop-file-modtime)

;;
;;; Helpers

;;;###autoload
(defun my-session-file (&optional name)
  "TODO"
  (cond ((require 'persp-mode nil t)
         (expand-file-name (or name persp-auto-save-fname) persp-save-dir))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))

;;;###autoload
(defun my-save-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (my-session-file))))
  (cond ((require 'persp-mode nil t)
         (unless persp-mode (persp-mode +1))
         (setq persp-auto-save-opt 0)
         (persp-save-state-to-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

;;;###autoload
(defun my-load-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (my-session-file))))
  (message "Attempting to load %s" file)
  (cond ((not (file-readable-p file))
         (message "No session file at %S to read from" file))
        ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (let ((allowed (persp-list-persp-names-in-file file)))
           (cl-loop for name being the hash-keys of *persp-hash*
                    unless (member name allowed)
                    do (persp-kill name))
           (persp-load-state-from-file file)))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))
;;;###autoload
(defun my/restart ()
  "Restart Emacs (and the daemon, if active).

Unlike `my/restart-and-restore', does not restart the current session."
  (interactive)
  (require 'restart-emacs)
  (restart-emacs))

;;;###autoload
(defun my/quicksave-session ()
  "TODO"
  (interactive)
  (message "Saving session")
  (my-save-session)
  (message "Saving session...DONE"))

;;;###autoload
(defun my/restart-and-restore (&optional debug)
  "Restart Emacs (and the daemon, if active).

If DEBUG (the prefix arg) is given, start the new instance with the --debug
switch."
  (interactive "P")
  (require 'restart-emacs)
  (my/quicksave-session)
  (save-some-buffers nil t)
  (letf! ((#'save-buffers-kill-emacs #'kill-emacs)
          (confirm-kill-emacs)
          (tmpfile (make-temp-file "post-load")))
    ;; HACK `restart-emacs' does not properly escape arguments on Windows (in
    ;;   `restart-emacs--daemon-on-windows' and
    ;;   `restart-emacs--start-gui-on-windows'), so don't give it complex
    ;;   arguments at all. Should be fixed upstream, but restart-emacs seems to
    ;;   be unmaintained.
    (with-temp-file tmpfile
      (print `(progn
                (add-hook 'window-setup-hook #'my-load-session 100)
                (delete-file ,tmpfile))
             (current-buffer)))
    (restart-emacs
     (append (if debug (list "--debug-init"))
             (when (boundp 'chemacs-current-emacs-profile)
               (list "--with-profile" chemacs-current-emacs-profile))
             (list "-l" tmpfile)))))
