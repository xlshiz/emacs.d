;;; vc.el --- define functions maybe used in version control -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +vc-git-get-current-file-relative-path ()
  "Get current file relative path."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;;;###autoload
(defun +vc/git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (+vc-git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (my/revert-buffer-no-confirm)
      (message "DONE! git checkout %s" filename))))

;;;###autoload
(defun +vc/git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (+vc-git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

;;;###autoload
(defun +vc-magit-display-buffer-fn (buffer)
  (if magit-display-buffer-noselect
      ;; the code that called `magit-display-buffer-function'
      ;; expects the original window to stay alive, we can't go
      ;; fullscreen
      (magit-display-buffer-traditional buffer)
    (delete-other-windows)
    ;; make sure the window isn't dedicated, otherwise
    ;; `set-window-buffer' throws an error
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    ;; return buffer's window
    (get-buffer-window buffer)))

;;;###autload
(defun +vc-magit-bury-buffer-fn (&rest _)
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (magit-restore-window-configuration)
  (let ((buffers (magit-mode-get-buffers)))
    (when (eq major-mode 'magit-status-mode)
      (mapc (lambda (buf)
              (with-current-buffer buf
                (if (and magit-this-process
                         (eq (process-status magit-this-process) 'run))
                    (bury-buffer buf)
                  (kill-buffer buf))))
            buffers))))

(defvar +vc--magit-stale-p nil)

(defun +vc--magit-revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+vc--magit-stale-p)
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun +vc-magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (+vc--magit-revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local +vc--magit-stale-p t))))))

;;;###autoload
(defun +vc/magit-quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+vc/magit-quit-all))))

;;;###autoload
(defun +vc/magit-quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+vc--magit-kill-buffer (magit-mode-get-buffers))
  (+vc-magit-mark-stale-buffers-h))

(defun +vc--magit-kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+vc--magit-kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))
