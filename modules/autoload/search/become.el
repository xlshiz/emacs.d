;;; editor/search/autoload/become.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +embark-find-file (&rest _)
  (quit-minibuf-and-run! (call-interactively 'find-file)))

;;;###autoload
(defun +embark-find-file-cwd (&optional input)
  "Perform a recursive file search from the current directory."
  (embark--quit-and-run #'+consult--fd :dir default-directory :initial input))

;;;###autoload
(defun +embark-find-file-other-dir (&optional input)
  "Perform a recursive file search from the current directory."
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (expand-file-name (read-directory-name "Search directory: "))))
    (embark--quit-and-run #'+consult--fd :dir default-directory :initial input)))

;;;###autoload
(defun +embark-find-file-other-project (&optional input)
  "Perform a recursive file search from the current directory."
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory
           (if-let* ((projects (projectile-relevant-known-projects)))
               (completing-read "Search project: " projects nil t)
             (user-error "There are no known projects"))))
    ;;NOTE!!
    (embark--quit-and-run
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda ()
             (delete-minibuffer-contents)
             (insert input))
         (+consult--fd :dir default-directory))))))

;;;###autoload
(defun +embark-clean-input(input)
  (if (string= (substring input 0 1) "#")
      (substring input 1)
    input))

;;;###autoload
(defun +embark-grep-buffer (input)
  (embark--quit-and-run #'consult-line (+embark-clean-input input)))

;;;###autoload
(defun +embark-grep-other-dir (input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (expand-file-name (read-directory-name "Search directory: "))))
    (setq this-command #'+embark-grep-other-dir)
    (embark--quit-and-run #'+consult--grep :query (+embark-clean-input input) :in default-directory)))

;;;###autoload
(defun +embark-grep-other-project (input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory
          (if-let* ((projects (projectile-relevant-known-projects)))
              (completing-read "Search project: " projects nil t)
            (user-error "There are no known projects"))))
    (setq this-command #'+embark-grep-other-project)
    (embark--quit-and-run #'+consult/grep-project nil (+embark-clean-input input) default-directory)))
