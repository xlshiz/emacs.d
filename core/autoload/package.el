;;; package.el --- define functions used in package manage -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;
;;; Incremental lazy-loading

(defvar my-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:

  (my-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`my-load-packages-incrementally-h' from `emacs-startup-hook' or set
`my-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar my-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.
Set this to 0 to load all incrementally deferred packages immediately at
`emacs-startup-hook'.")

(defvar my-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

;;;###autoload
(defun my-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `my-incremental-idle-timer'
intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append my-incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              ;; (message "start:iloader: Already loaded %s (%d left)" req (length packages))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) my-incremental-first-idle-timer)
                     (not
                      (while-no-input
                        ;; (message "start:iloader: Loading %s (%d left)" req (length packages))
                        ;; If `default-directory' doesn't exist or is
                        ;; unreadable, Emacs throws file errors.
                        (let ((default-directory user-emacs-directory)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
                ;; (message "start:iloader: Finished!")
              (run-at-time (if idle-time
                               my-incremental-idle-timer
                             my-incremental-first-idle-timer)
                           nil #'my-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

;;;###autoload
(defun my-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `my-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp my-incremental-first-idle-timer)
    (if (zerop my-incremental-first-idle-timer)
        (mapc #'require (cdr my-incremental-packages))
      (run-with-idle-timer my-incremental-first-idle-timer
                           nil #'my-load-packages-incrementally
                           (cdr my-incremental-packages) t))))
;;;###autoload
(defun my/install-pkg (pkgs)
  (let* ((pkgs-list (if (listp pkgs)
                       pkgs
                      (list pkgs)))
         pkg url)
    (require 'epkg)
    (dolist (el pkgs-list)
      (setq pkg (epkg (symbol-name el)))
      (setq url (and pkg
                     (if (or (epkg-git-package-p pkg)
                             (epkg-github-package-p pkg)
                             (epkg-orphaned-package-p pkg)
                             (epkg-gitlab-package-p pkg))
                       (eieio-oref pkg 'url)
                       (eieio-oref pkg 'mirror-url))))
      (when url
        (pcase-dolist (`(,orig . ,base) borg-rewrite-urls-alist)
                      (when (string-prefix-p orig url)
                        (setq url (concat base (substring url (length orig)))))))
      (when url
        (print el)
        (condition-case err
                        (borg-assimilate (symbol-name el) url)
                        (error                        ; Condition.
                          (message "%s" (error-message-string err))))))))
