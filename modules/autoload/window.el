;;; window.el --- window releated functions. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:


;;;###autoload
(defun +win/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun +win/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun +win/toggle-golden-ratio ()
  "Golden ratio mode toggle function."
  (interactive)
  (if golden-ratio-mode
      (progn
        (golden-ratio-mode -1)
        (message "golden ratio disabled")
        (balance-windows))
    (golden-ratio-mode 1)
    (message "golden ratio enabled")
    (golden-ratio)))

;;;###autoload
;; https://github.com/redguardtoo/emacs.d/blob/ff06a775ef694970136fb8d26d06a339fb410d9c/lisp/init-windows.el#L29
(defun +win/toggle-two-split-window ()
  "Toggle two window layout vertically or horizontally.
Windows whose `no-other-window' parameter is non-nil (e.g. the
xsort-tab tab bar window) are ignored, so `count-windows' counting
such a window doesn't break the toggle; `other-window' and
`delete-other-windows' already skip/keep them."
  (interactive)
  (let* ((sel (selected-window))
         (wins (delq nil (mapcar (lambda (w)
                                   (unless (window-parameter w 'no-other-window)
                                     w))
                                 (window-list (selected-frame) 1)))))
    (when (and (= (length wins) 2) (memq sel wins))
      (let* ((this-win-buffer (window-buffer sel))
             (next-win (other-window 1))
             (next-win-buffer (window-buffer next-win))
             (this-win-edges (window-edges sel))
             (next-win-edges (window-edges next-win))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges) (car next-win-edges))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (when this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (when this-win-2nd (other-window 1)))))))
