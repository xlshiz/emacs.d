;;; edit/color-rg.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +search/color-rg-jump-next-keyword ()
  (interactive)
  (let* ((next-position (color-rg-find-next-position color-rg-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (beginning-of-line)
          (forward-char (+ 1 (color-rg-get-match-column))))
      (message "Reach to last line."))))

;;;###autoload
(defun +search/color-rg-jump-prev-keyword ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp color-rg-regexp-position nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-position nil t)
                       (line-number-at-pos))))
               (if (equal first-search-line (line-number-at-pos))
                   ;; Search previous again if first search is same line of point.
                   (save-excursion
                     (beginning-of-line)
                     (search-backward-regexp color-rg-regexp-position nil t))
                 (save-excursion (search-backward-regexp color-rg-regexp-position nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (forward-char (+ 1 (color-rg-get-match-column))))
      (message "Reach to first line."))))

;;;###autoload
(defun +search/swiper-to-color-rg ()
  (interactive)
  (let ((search-text
         (replace-regexp-in-string
          "\n" ""
          (replace-regexp-in-string
           "\\\\_<" ""
           (replace-regexp-in-string
            "\\\\_>" ""
            (replace-regexp-in-string "^.*Swiper: " "" (thing-at-point 'line)))))))
    (ivy-quit-and-run (color-rg-search-input search-text (expand-file-name (buffer-file-name))))))

;;;###autoload
(defun +search/counsel-to-color-rg ()
  (interactive)
  (let ((search-text
         (replace-regexp-in-string
          "\n" ""
          (replace-regexp-in-string "^.*Search project \\[.*\\]: " "" (thing-at-point 'line)))))
    (ivy-quit-and-run (color-rg-search-input search-text default-directory))))

;;;###autoload
(defmacro +search-minibuf-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  (declare (indent 0))
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@body)))
     (abort-recursive-edit)))

;;;###autoload
(defun +search/consult-to-color-rg ()
  (interactive)
  (let ((search-text (minibuffer-contents-no-properties)))
    (if (equal (substring search-text 0 1) "#")
        (+search-minibuf-quit-and-run (color-rg-search-input (substring search-text 1) (my-project-root)))
      (+search-minibuf-quit-and-run (color-rg-search-input search-text (expand-file-name (buffer-file-name)))))))

;;;###autoload
(defun evil-collection-color-rg-setup ()
  "Set up `evil' bindings for `color-rg'."
  (eval-when-compile (require 'evil-collection))
  (evil-collection-define-key 'normal 'color-rg-mode-map
    (kbd "M-o") 'color-rg-hydra/body
    (kbd "RET") 'color-rg-open-file-and-stay
    (kbd "C-.") 'color-rg-open-file
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char

    "n" '+search/color-rg-jump-next-keyword
    "p" '+search/color-rg-jump-prev-keyword
    "N" 'color-rg-jump-next-keyword
    "P" 'color-rg-jump-prev-keyword
    "H" 'color-rg-jump-next-file
    "L" 'color-rg-jump-prev-file

    "r" 'color-rg-replace-all-matches
    "f" 'color-rg-filter-match-results
    "F" 'color-rg-filter-mismatch-results

    "x" 'color-rg-filter-match-files
    "X" 'color-rg-filter-mismatch-files
    "u" 'color-rg-unfilter

    "D" 'color-rg-remove-line-from-results

    "I" 'color-rg-rerun-toggle-ignore
    "t" 'color-rg-rerun-literal
    "c" 'color-rg-rerun-toggle-case
    "s" 'color-rg-rerun-regexp
    "d" 'color-rg-rerun-change-dir
    "z" 'color-rg-rerun-change-globs
    "Z" 'color-rg-rerun-change-exclude-files
    "C" 'color-rg-customized-search

    "i" 'color-rg-switch-to-edit-mode
    "q" 'color-rg-quit)

  (evil-collection-define-key 'normal 'color-rg-mode-edit-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char

    "n" 'color-rg-jump-next-keyword
    "p" 'color-rg-jump-prev-keyword
    "N" 'color-rg-jump-next-file
    "P" 'color-rg-jump-prev-file)

  (evil-collection-define-key nil 'color-rg-mode-edit-map
    [remap evil-write] 'color-rg-apply-changed
    [remap evil-quit] 'color-rg-quit)

  (evil-set-initial-state 'color-rg-mode 'normal))
