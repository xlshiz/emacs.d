;;; files.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defun my-files--build-checks (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (my-files--build-checks
    '(or A (and B C))
    \"~\")

Returns (not precisely, but effectively):

  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))

This is used by `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (if (and (listp spec)
           (memq (car spec) '(or and)))
      (cons (car spec)
            (cl-loop for it in (cdr spec)
                     collect (my-files--build-checks it directory)))
    (let ((filevar (make-symbol "file")))
      `(let ((,filevar ,spec))
         (and (stringp ,filevar)
              ,(if directory
                   `(let ((default-directory ,directory))
                      (file-exists-p ,filevar))
                 `(file-exists-p ,filevar))
              ,filevar)))))

;;;###autoload
(defmacro file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(my-files--build-checks files directory)))
     (and p (expand-file-name p ,directory))))

;;;###autoload
(defun my/rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      (kill-buffer)
      (find-file file-to))))

;;;###autoload
(defun my/copy-file()
  "Copy file while using current file as default."
  (interactive)
  (copy-file
   (read-file-name "Copy from: " default-directory buffer-file-name)
   (read-file-name "Copy to:" default-directory)))

;;;###autoload
(defun my/delete-file()
  "Delete file while using current file as default."
  (interactive)
  (let ((file-name (read-file-name "Delete: " default-directory (buffer-file-name))))
    (cond
     ((file-directory-p file-name) (delete-directory file-name t))
     ((file-exists-p file-name) (delete-file file-name))
     (t (message "Not found!")))
    (unless (file-exists-p (buffer-file-name))
      (kill-current-buffer))))

