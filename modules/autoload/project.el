;;; projectile.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; HACK We forward declare these variables because they are let-bound in a
;;      number of places with no guarantee that they've been defined yet (i.e.
;;      that `projectile' is loaded). If a variable is defined with `defvar'
;;      while it is lexically bound, you get "Defining as dynamic an already
;;      lexical var" errors in Emacs 28+).
;;;###autoload (defvar projectile-project-root nil)
;;;###autoload (defvar projectile-enable-caching (not noninteractive))
;;;###autoload (defvar projectile-require-project-root 'prompt)

;;
;;; Library

;;;###autoload
(defun +project-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (+project-project-root dir)
       t))

;;;###autoload
(defun +project-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun +project-project-name (&optional dir)
  "Return the name of the current project.

Returns '-' if not in a valid project."
  (if-let (project-root (or (+project-project-root dir)
                            (if dir (expand-file-name dir))))
      (funcall projectile-project-name-function project-root)
    "-"))

;;;###autoload
(defun +project-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (+project-project-root dir)))

;;;###autoload
(defmacro +project-file-exists-p! (files &optional base-directory)
  "Checks if FILES exist at the current project's root.

The project's root is determined by `projectile', starting from BASE-DIRECTORY
(defaults to `default-directory'). FILES are paths relative to the project root,
unless they begin with a slash."
  `(file-exists-p! ,files (+project-project-root ,base-directory)))

;;;###autoload
(defun +project-ignored-p (project-root)
  "Return non-nil if temporary file or a straight package."
  (unless (file-remote-p project-root)
    (or (file-in-directory-p project-root temporary-file-directory)
        (file-in-directory-p project-root (concat user-emacs-directory "lib")))))

;;;###autoload
(defun +project-find-file (dir)
  "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename dir))
         (projectile-project-root (+project-project-root dir))
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and projectile-project-root (file-equal-p projectile-project-root default-directory))
           (unless (+project-project-p default-directory)
             ;; Disable caching if this is not a real project; caching
             ;; non-projects easily has the potential to inflate the projectile
             ;; cache beyond reason.
             (setq projectile-enable-caching nil))
           (call-interactively
            ;; Intentionally avoid `helm-projectile-find-file', because it runs
            ;; asynchronously, and thus doesn't see the lexical
            ;; `default-directory'
            #'projectile-find-file))
          ((and (bound-and-true-p vertico-mode)
                (fboundp '+vertico/find-file-in))
           (+vertico/find-file-in default-directory))
          ((project-current nil dir)
           (project-find-file-in nil nil dir))
          ((call-interactively #'find-file)))))
