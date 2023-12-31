;;; format.el --- autoload functions used for format buffer. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/clang-format-region-or-buffer ()
  "Format the current region or buffer with clang-format.
if .clang-format exists in the projectile root, Otherwise,
use google style by default"
  (interactive)
  (save-excursion
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (setq clang-format-style-option "file"))
    (if (region-active-p)
        (clang-format-region (region-beginning) (region-end))
      (clang-format-buffer))))
