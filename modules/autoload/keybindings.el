;;; keybindings.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively #'+vertico/project-search-from-cwd)))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (if (and start end (not multiline-p))
        (consult-line
          (replace-regexp-in-string
            " " "\\\\ "
            (rxt-quote-pcre
              (buffer-substring-no-properties start end))))
        (call-interactively #'consult-line)))))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively #'+vertico/project-search)))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (my-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (+project-project-root default-directory)))))
  (+vertico/project-search nil symbol dir))

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (my-thing-at-point-or-region) ""))))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   symbol org-directory))

;;;###autoload
(defun +default/org-notes-search (query)
  "Perform a text search on `org-directory'."
  (interactive
   (list (if (my-region-active-p)
             (buffer-substring-no-properties
              (my-region-beginning)
              (my-region-end))
           "")))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   query org-directory))

;;;###autoload
(defun +default/evil-search-to-project (&optional arg symbol)
  "Conduct a text search in the current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list current-prefix-arg
         (replace-regexp-in-string
          "\\\\" ""
          (replace-regexp-in-string
           "\n" ""
           (replace-regexp-in-string
            "\\\\_<" ""
            (replace-regexp-in-string
             "\\\\_>" ""
             (car evil-ex-search-history)))))))
  (let ((default-directory
          (if arg
              (if-let* ((projects (projectile-relevant-known-projects)))
                  (completing-read "Switch to project: " projects
                                   nil t nil nil (+project-project-root))
                (user-error "There are no known projects"))
            default-directory)))
    (+vertico/project-search nil (rxt-quote-pcre symbol))))

;;;###autoload
(defun +default/find-in-notes ()
  "Find a file under `org-directory', recursively."
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (+project-find-file org-directory))

;;;###autoload
(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (+project-find-file default-directory))

;;;###autoload
(defun +default/discover-projects (arg)
  "Discover projects in `projectile-project-search-path'.
If prefix ARG is non-nil, prompt for the search path."
  (interactive "P")
  (if arg
      (call-interactively #'projectile-discover-projects-in-directory)
    (if (not projectile-project-search-path)
        (user-error "`projectile-project-search-path' is empty; don't know where to search")
      (letf! (defun projectile-add-known-project (project-root)
               (unless (projectile-ignored-project-p project-root)
                 (funcall projectile-add-known-project project-root)
                 (message "Added %S to known project roots" project-root)))
        (dolist (dir projectile-project-search-path)
          (cl-destructuring-bind (dir . depth) (if (consp dir) dir (cons dir nil))
            (if (not (file-accessible-directory-p dir))
                (message "%S was inaccessible and couldn't be searched" dir)
              (projectile-discover-projects-in-directory dir depth))))))))

;;;###autoload
(defun +default/dired (arg)
  "Open a directory in dired.
If prefix ARG is non-nil, prompt for a known project to open in dired."
  (interactive "P")
  (apply #'dired
         (if arg
             (list (completing-read "Open dired in project: " projectile-known-projects))
           (dired-read-dir-and-switches ""))))

;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +default/man-at-point ()
  "Invoke `man' if man is installed and the platform is not MacOS, otherwise use `woman'.

`man -k \"^\"` is very slow on MacOS, which is what `Man-completion-table' uses to
generate `completing-read' candidates."
  (interactive)
  (consult-man (my-thing-at-point-or-region)))
