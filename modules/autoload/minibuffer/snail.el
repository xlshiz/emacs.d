;;; snail.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun snail--project-files (&rest _)
  (require 'projectile)
  (let* ((ht (consult--buffer-file-hash))
          (candidates (unless (file-equal-p default-directory "~")
                        (mapc #'substring-no-properties (if (+project-project-root)
                                                          (projectile-current-project-files))))))
    (if (length> candidates 100)
      candidates
      (seq-remove (lambda (x) (gethash (projectile-expand-root x) ht)) candidates))))

;;;###autoload
(defun snail--buffer-exclude (buf)
  (catch 'failed
    (dolist (whitelist-buf '("*scratch*" "*Messages*"))
      (when (string-prefix-p whitelist-buf buf)
        (throw 'failed nil)))
    (dolist (backlist-buf '(" *" "*"
                            " markdown-code-fontification"
                            " tq-temp-epdfinfo"))
      (when (string-prefix-p backlist-buf buf)
        (throw 'failed t)))
    nil))

;;;###autoload
(defun snail--buffers (&rest _)
  (let ((buffers (buffer-list)))
    (setq buffers (funcall (intern "consult--buffer-sort-visibility") buffers))
    (setq buffers (cl-map 'list #'buffer-name buffers))
    (seq-remove #'snail--buffer-exclude buffers)))

;;;###autoload
(defvar snail--source-buffer
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items    snail--buffers)
  "Buffer candidate source for `snail'.")

;;;###autoload
(defvar snail--source-hidden-buffer
  `(:name     "Hidden Buffer"
    :narrow   32
    :hidden   t
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :items
    ,(lambda () (cl-set-difference
                  (consult--buffer-query :as #'buffer-name :exclude nil)
                  (snail--buffers))))
  "Buffer candidate source for `snail'.")

;;;###autoload
(defvar snail--source-recent-file
  `(:name "Recent Files"
    :narrow (?r . "Recent")
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'consult--file-state
    :action nil
    :require-match nil
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash)))
         (recentf-mode)
         (mapcar #'abbreviate-file-name
                 (seq-remove (lambda (x)
                               (gethash x ht))
                             recentf-list)))))
  "Recent file candidate source for `snail'.")

;;;###autoload
(defvar snail--source-project-file
  `(:name     "Project File"
    :narrow   (?p . "Project")
    :category file
    :hidden   nil
    :action   nil
    :require-match nil
    :items    snail--project-files)
  "Project buffer candidate source for `snail'.")

;;;###autoload
(defvar snail-sources
  '(snail--source-project-file
    snail--source-buffer
    snail--source-recent-file
    ;; hiden
    snail--source-hidden-buffer
    consult--source-modified-buffer
    ))

;;;###autoload
(defun snail ()
  "Mix the `buffer' `projectile' and `recentf'"
  (interactive)
  (require 'consult)
  (when-let (sel (consult--multi snail-sources
                                 :require-match nil
                                 :prompt "Snail: "
                                 :history nil
                                 :sort nil))
    (if (cdr sel)
        (if (string= (plist-get (cdr sel) :name) "Project File")
            (find-file (projectile-expand-root (car sel))))
      (consult--buffer-action (car sel)))))
