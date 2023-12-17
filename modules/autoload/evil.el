;;; evil.el --- autoload functions used for format buffer. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload (autoload '+evil:whole-buffer-txtobj "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))

;;;###autoload (autoload '+evil:defun-txtobj "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
  "Text object to select the top-level Lisp form or function definition at
point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end type)))

;;;###autoload (autoload '+evil:inner-url-txtobj "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
  "Text object to select the inner url at point.

This excludes the protocol and querystring."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     (save-excursion
       (goto-char beg)
       (re-search-forward "://" end t))
     (save-excursion
       (goto-char end)
       (- (if-let (pos (re-search-backward "[?#]" beg t))
              pos
            end)
          (if (evil-visual-state-p)
              1
            0)))
     type)))

;;;###autoload (autoload '+evil:outer-url-txtobj "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
  "Text object to select the whole url at point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     beg (- end (if (evil-visual-state-p) 1 0))
     type)))

;;;###autoload (autoload '+evil:inner-any-quote "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:inner-any-quote (count &optional beg end type)
  "Select the closest inner quote."
  (require 'evil-textobj-anyblock)
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "`")
           ("‘" . "’")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count nil)))

;;;###autoload (autoload '+evil:outer-any-quote "../modules/autoload/evil" nil nil)
(evil-define-text-object +evil:outer-any-quote (count &optional beg end type)
  "Select the closest outer quote."
  (require 'evil-textobj-anyblock)
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "`")
           ("‘" . "’")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count t)))
;;
;;; wgrep

;;;###autoload (autoload '+evil-delete "../modules/autoload/evil" nil t)
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case _ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))
