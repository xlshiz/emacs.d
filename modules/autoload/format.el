;;; format.el --- autoload functions used for format buffer. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

(defun +format-region (start end &optional callback)
  "Format from START to END with `apheleia'."
  (when-let* ((command (apheleia--get-formatters
                        (if current-prefix-arg
                            'prompt
                          'interactive)))
              (cur-buffer (current-buffer))
              (formatted-buffer (get-buffer-create " *apheleia-formatted*"))
              (indent 0))
    (with-current-buffer formatted-buffer
      (erase-buffer)
      (unless is-windows-p
        (setq-local coding-system-for-read 'utf-8)
        (setq-local coding-system-for-write 'utf-8))
      ;; Ensure this temp buffer seems as much like the origin buffer as
      ;; possible, in case the formatter is an elisp function, like `gofmt'.
      (cl-loop for (var . val)
               in (cl-remove-if-not #'listp (buffer-local-variables cur-buffer))
               ;; Making enable-multibyte-characters buffer-local causes an
               ;; error.
               unless (eq var 'enable-multibyte-characters)
               ;; Using setq-local would quote var.
               do (set (make-local-variable var) val))
      ;;
      (insert-buffer-substring-no-properties cur-buffer start end)
      ;; Since we're piping a region of text to the formatter, remove any
      ;; leading indentation to make it look like a file.
      (setq indent (+format--current-indentation))
      (when (> indent 0)
        (indent-rigidly (point-min) (point-max) (- indent)))
      ;;
      (apheleia-format-buffer
       command
       (lambda ()
         (with-current-buffer formatted-buffer
           (when (> indent 0)
             ;; restore indentation without affecting new
             ;; indentation
             (indent-rigidly (point-min) (point-max)
                             (max 0 (- indent (+format--current-indentation)))))
           (set-buffer-modified-p nil))
         (with-current-buffer cur-buffer
           (delete-region start end)
           (insert-buffer-substring-no-properties formatted-buffer)
           (when callback (funcall callback))
           (kill-buffer formatted-buffer)))))))


;;
;;; Commands

;;;###autoload
(defun +format/buffer (&optional arg)
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive "P")
  (call-interactively
    #'apheleia-format-buffer))

;;;###autoload
(defun +format/region (beg end &optional arg)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (+format-region beg end))

;;;###autoload
(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (my-region-active-p)
       #'+format/region
     #'+format/buffer)))

;;;###autoload
(cl-defun set-formatter! (name args &key modes)
  "Define (or modify) a formatter named NAME.

Supported keywords: :modes

NAME is a symbol that identifies this formatter.

FORMATTER can be a symbol referring to another formatter, a function, string or
nested list.

  If a function, it should be a formatter function that
    `apheleia--run-formatter-function' will accept.
  If a string, it is assumed to be a shell command that the buffer's text will
    be piped to (through stdin).
  If a list, it should represent a shell command as a list of arguments. Each
    element is either a string or list (STRING ARG) where STRING is a format
    string and ARG is both a predicate and argument for STRING. If ARG is nil,
    STRING will be omitted from the vector.

If you're trying to override this, ensure that you wrap the call in `after!' and
whichever package sets the initial formatter. See the ':editor format' README
for more.

For more information on how to structure the list to be compatible, see
`apheleia--run-formatter-function'.

MODES is a major mode, a list thereof, or a list of two-element sublists with
the structure: (MAJOR-MODE FORM). FORM is evaluated when the buffer is formatted
and its return value serves two purposes:

  1. It is a predicate for this formatter. Assuming the MAJOR-MODE matches the
     current mode, if FORM evaluates to nil, the formatter is skipped.
  2. It's return value is made available to FORMATTER if it is a function or
     list of shell arguments via the `mode-result' variable.

Basic examples:
  (set-formatter! \\='asmfmt \"asmfmt\" :modes \\='(asm-mode nasm-mode))
  (set-formatter! \\='black \"black -q -\")
  (set-formatter! \\='html-tidy \"tidy -q -indent\" :modes \\='(html-mode web-mode))

Advanced examples:
  (set-formatter!
    \\='clang-format
    \\='(\"clang-format\"
      (\"-assume-filename=%S\" (or buffer-file-name mode-result \"\")))
    :modes
    \\='((c-mode \".c\")
      (c++-mode \".cpp\")
      (java-mode \".java\")
      (objc-mode \".m\")
      (protobuf-mode \".proto\")))

  (set-formatter! \\='html-tidy
    \\='(\"tidy\" \"-q\" \"-indent\"
      (\"-xml\" (memq major-mode \\='(nxml-mode xml-mode))))
    :modes
    \\='(html-mode
      (web-mode (and (equal \"none\" web-mode-engine)
                     (car (member web-mode-content-type \\='(\"xml\" \"html\")))))))

  (set-formatter! \\='html-tidy  ; overwrite predefined html-tidy formatter
    \\='(\"tidy\" \"-q\" \"-indent\"
      \"--tidy-mark\" \"no\"
      \"--drop-empty-elements\" \"no\"
      \"--show-body-only\" \"auto\"
      (\"--indent-spaces\" \"%d\" tab-width)
      (\"--indent-with-tabs\" \"%s\" (if indent-tabs-mode \"yes\" \"no\"))
      (\"-xml\" (memq major-mode \\='(nxml-mode xml-mode)))))

  (set-formatter! \\='elm-format
    \"elm-format --yes --stdin\")"
  (declare (indent defun))
  (cl-check-type name symbol)
  (after! apheleia
    (if (null args)
        (progn
          (setq apheleia-formatters
                (assq-delete-all name apheleia-formatters))
          (while (rassoc name apheleia-mode-alist)
            (setq apheleia-mode-alist
                  (assq-delete-all (car (rassoc name apheleia-mode-alist)) apheleia-mode-alist))))
      (let ((formatter (cond
                        ((listp args) `(,@args))
                        (t args))))
        (setf (alist-get name apheleia-formatters) formatter))
      (when modes
        (dolist (mode modes)
          (setf (alist-get mode apheleia-mode-alist) name))))))
