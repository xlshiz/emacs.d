;;; buffer.el --- autoload functions used in buffer. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun my/alternate-buffer-in-persp ()
  "Switch back and forth between current and last buffer in the
  current perspective."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

;;;###autoload
(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

;; Kill all buffers except scratch buffer
;;;###autoload
(defun my/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; Kill all buffers except the current one.
;;;###autoload
(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun my/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

;;;###autoload
(defun my-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (ensure-list modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (apply #'provided-mode-derived-p
                                   (buffer-local-value 'major-mode buf)
                                   modes))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (buffer-list)))))
