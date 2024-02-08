;;; edit.el --- autoload functions used in edit. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defvar my-point-in-comment-functions ()
  "List of functions to run to determine if point is in a comment.

Each function takes one argument: the position of the point. Stops on the first
function to return non-nil. Used by `my-point-in-comment-p'.")

;;;###autoload
(defvar my-point-in-string-functions ()
  "List of functions to run to determine if point is in a string.

Each function takes one argument: the position of the point. Stops on the first
function to return non-nil. Used by `my-point-in-string-p'.")

;;;###autoload
(defun my-surrounded-p (pair &optional inline balanced)
  "Returns t if point is surrounded by a brace delimiter: {[(

If INLINE is non-nil, only returns t if braces are on the same line, and
whitespace is balanced on either side of the cursor.

If INLINE is nil, returns t if the opening and closing braces are on adjacent
lines, above and below, with only whitespace in between."
  (when pair
    (let ((beg (plist-get pair :beg))
          (end (plist-get pair :end))
          (pt (point)))
      (when (and (> pt beg) (< pt end))
        (when-let* ((cl (plist-get pair :cl))
                    (op (plist-get pair :op)))
          (and (not (string= op ""))
               (not (string= cl ""))
               (let ((nbeg (+ (length op) beg))
                     (nend (- end (length cl))))
                 (let ((content (buffer-substring-no-properties nbeg nend)))
                   (and (string-match-p (format "[ %s]*" (if inline "" "\n")) content)
                        (or (not balanced)
                            (= (- pt nbeg) (- nend pt))))))))))))

;;;###autoload
(defun my-point-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
POS defaults to the current position."
  (let ((pos (or pos (point))))
    (if my-point-in-comment-functions
        (run-hook-with-args-until-success 'my-point-in-comment-functions pos)
      (nth 4 (syntax-ppss pos)))))

;;;###autoload
(defun my-point-in-string-p (&optional pos)
  "Return non-nil if POS is in a string."
  ;; REVIEW Should we cache `syntax-ppss'?
  (let ((pos (or pos (point))))
    (if my-point-in-string-functions
        (run-hook-with-args-until-success 'my-point-in-string-functions pos)
      (nth 3 (syntax-ppss pos)))))

;;;###autoload
(defun my-point-in-string-or-comment-p (&optional pos)
  "Return non-nil if POS is in a string or comment."
  (or (my-point-in-string-p pos)
      (my-point-in-comment-p pos)))

;;;###autoload
(defun my-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun my-region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

;;;###autoload
(defun my-region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (if (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))
      evil-visual-end
    (region-end)))

;;;###autoload
(defun my-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((my-region-active-p)
         (buffer-substring-no-properties
          (my-region-beginning)
          (my-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))


;;
;;; Commands

(defun my--bol-bot-eot-eol (&optional pos)
  (save-mark-and-excursion
    (when pos
      (goto-char pos))
    (let* ((bol (if visual-line-mode
                    (save-excursion
                      (beginning-of-visual-line)
                      (point))
                  (line-beginning-position)))
           (bot (save-excursion
                  (goto-char bol)
                  (skip-chars-forward " \t\r")
                  (point)))
           (eol (if visual-line-mode
                    (save-excursion (end-of-visual-line) (point))
                  (line-end-position)))
           (eot (or (save-excursion
                      (if (not comment-use-syntax)
                          (progn
                            (goto-char bol)
                            (when (re-search-forward comment-start-skip eol t)
                              (or (match-end 1) (match-beginning 0))))
                        (goto-char eol)
                        (while (and (my-point-in-comment-p)
                                    (> (point) bol))
                          (backward-char))
                        (skip-chars-backward " " bol)
                        (or (eq (char-after) 32)
                            (eolp)
                            (bolp)
                            (forward-char))
                        (point)))
                    eol)))
      (list bol bot eot eol))))

(defvar my--last-backward-pt nil)
;;;###autoload
(defun my/backward-to-bol-or-indent (&optional point)
  "Jump between the indentation column (first non-whitespace character) and the
beginning of the line. The opposite of
`my/forward-to-last-non-comment-or-eol'."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (bol bot _eot _eol)
        (my--bol-bot-eot-eol pt)
      (cond ((> pt bot)
             (goto-char bot))
            ((= pt bol)
             (or (and my--last-backward-pt
                      (= (line-number-at-pos my--last-backward-pt)
                         (line-number-at-pos pt)))
                 (setq my--last-backward-pt nil))
             (goto-char (or my--last-backward-pt bot))
             (setq my--last-backward-pt nil))
            ((<= pt bot)
             (setq my--last-backward-pt pt)
             (goto-char bol))))))

(defvar my--last-forward-pt nil)
;;;###autoload
(defun my/forward-to-last-non-comment-or-eol (&optional point)
  "Jumps between the last non-blank, non-comment character in the line and the
true end of the line. The opposite of `my/backward-to-bol-or-indent'."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (_bol _bot eot eol)
        (my--bol-bot-eot-eol pt)
      (cond ((< pt eot)
             (goto-char eot))
            ((= pt eol)
             (goto-char (or my--last-forward-pt eot))
             (setq my--last-forward-pt nil))
            ((>= pt eot)
             (setq my--last-backward-pt pt)
             (goto-char eol))))))

;;;###autoload
(defun my/backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again afterwards, kill
line to beginning of line. Same as `evil-delete-back-to-indentation'."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (funcall (if (fboundp 'evil-delete)
                 #'evil-delete
               #'delete-region)
             (point-at-bol) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun my/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;###autoload
(defun my/dumb-indent ()
  "Inserts a tab character (or spaces x tab-width)."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

;;;###autoload
(defun my/dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

;;;###autoload
(defun my/retab (arg &optional beg end)
  "Converts tabs-to-spaces or spaces-to-tabs within BEG and END (defaults to
buffer start and end, to make indentation consistent. Which it does depends on
the value of `indent-tab-mode'.

If ARG (universal argument) is non-nil, retab the current buffer using the
opposite indentation style."
  (interactive "P\nr")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((indent-tabs-mode (if arg (not indent-tabs-mode) indent-tabs-mode)))
    (if indent-tabs-mode
        (tabify beg end)
      (untabify beg end))))

;;;###autoload
(defun my/delete-trailing-newlines ()
  "Trim trailing newlines.

Respects `require-final-newline'."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (delete-blank-lines)))

;;;###autoload
(defun my/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun my/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;###autoload
(defun my/toggle-indent-style ()
  "Switch between tabs and spaces indentation style in the current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

(defvar editorconfig-lisp-use-default-indent)
;;;###autoload
(defun my/set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.

The effectiveness of this command is significantly improved if you have
editorconfig or dtrt-indent installed."
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (setq tab-width width)
  (setq-local standard-indent width)
  (when (boundp 'evil-shift-width)
    (setq evil-shift-width width))
  (cond ((require 'editorconfig nil t)
         (let (editorconfig-lisp-use-default-indent)
           (editorconfig-set-indentation nil width)))
        ((require 'dtrt-indent nil t)
         (when-let (vars (nth 2 (assq major-mode dtrt-indent-hook-mapping-list)))
           (dolist (var (ensure-list vars))
             (set var width)))))
  (message "Changed indentation to %d" width))


;;
;;; Hooks

;;;###autoload
(defun my-enable-delete-trailing-whitespace-h ()
  "Enables the automatic deletion of trailing whitespaces upon file save.

i.e. enables `ws-butler-mode' in the current buffer."
  (ws-butler-mode +1))

;;;###autoload
(defun my-disable-delete-trailing-whitespace-h ()
  "Disables the automatic deletion of trailing whitespaces upon file save.

i.e. disables `ws-butler-mode' in the current buffer."
  (ws-butler-mode -1))

;;;###autoload
(defun my-enable-show-trailing-whitespace-h ()
  "Enable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace t))

;;;###autoload
(defun my-disable-show-trailing-whitespace-h ()
  "Disable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace nil))

;;;###autoload
(defun get-enc-char (c)
  (cond
   ((string= c "(") ")")
   ((string= c "[") "]")
   ((string= c "{") "}")
   ((string= c ">") "<")
   ((string= c "<") ">")
   ((string= c "'") "'")
   ((string= c "\"") "\"")
   (t nil)))
(defvar empty-enclose 0)

;;;###autoload
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

;;;###autoload
(defun my/delete-delimiter-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (setq empty-enclose 0)
  (save-excursion
    (let (p1 p2 orig)
      (setq orig (point))
      (setq p1 (point))
      (setq p2 (point))
      (setq find 0)
      (setq mychar (thing-at-point 'char))
      (if (-contains? '("(" "[" "{" "<" "'" "\"") mychar)
          (progn
            (setq left_encloser (thing-at-point 'char))
            (backward-char -1)
            (if (string-equal (thing-at-point 'char) (get-enc-char left_encloser))
                (progn
                  (backward-char -1)
                  (setq p2 (point))
                  (setq find 1)
                  (setq empty-enclose 1)))))
      (while (eq find 0)
        (skip-chars-backward "^({[<>\"'")
        (setq p1 (point))
        (backward-char 1)
        (setq left_encloser (thing-at-point 'char))
        (goto-char orig)
        (while (and (not (eobp)) (eq find 0))
          (backward-char -1)
          (skip-chars-forward "^)}]<>\"'")
          (setq right_encloser (thing-at-point 'char))
          (if (string-equal right_encloser (get-enc-char left_encloser))
              (progn
                (setq p2 (point))
                (setq find 1))))
        (goto-char p1)
        (backward-char 1))
      (delete-region p1 p2)))
  (if (eq empty-enclose 0)
      (backward-char 1)))

;;;###autoload
(defun my/buffer-too-big-p ()
  "Check if buffer size is larger than 1M or has more than 5000 lines."
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))

;;;###autoload
(defun my/open-init-file ()
  "Open emacs init file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun my/delete-word ()
  "Delete word under cursor."
  (interactive)
  (let ((end (get-point 'forward-word 1))
        (beg (get-point 'backward-word 1)))
    (delete-region beg end)))

;;;###autoload
(defun my/copy-word ()
  "print current word."
  (interactive)
  (kill-new (thing-at-point 'word)))

;;;###autoload
(defun my/cover-word ()
  "cover word before point"
  (interactive)
  (my/delete-word)
  (evil-paste-before 1))

;;;###autoload
(defun avy-goto-char-3 (char1 char2 char3 &optional arg beg end)
  "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive (list (let ((c1 (read-char "char 1: " t)))
                       (if (memq c1 '(? ?\b))
                           (keyboard-quit)
                         c1))
                     (let ((c2 (read-char "char 2: " t)))
                       (cond ((eq c2 ?)
                              (keyboard-quit))
                             ((memq c2 avy-del-last-char-by)
                              (keyboard-escape-quit)
                              (call-interactively 'avy-goto-char-3))
                             (t
                              c2)))
                     (let ((c3 (read-char "char 3: " t)))
                       (cond ((eq c3 ?)
                              (keyboard-quit))
                             ((memq c3 avy-del-last-char-by)
                              (keyboard-escape-quit)
                              (call-interactively 'avy-goto-char-3))
                             (t
                              c3)))
                     current-prefix-arg
                     nil nil))
  (when (eq char1 ?)
    (setq char1 ?\n))
  (when (eq char2 ?)
    (setq char2 ?\n))
  (when (eq char3 ?)
    (setq char3 ?\n))
  (avy-with avy-goto-char-3
    (avy-jump
     ;; (regexp-quote (string char1 char2 char3))
     (pinyinlib-build-regexp-string (string char1 char2 char3)
                                    (not ace-pinyin-enable-punctuation-translation)
                                    (not ace-pinyin-simplified-chinese-only-p))
     :window-flip arg
     :beg beg
     :end end)))

;;;###autoload
(defun my/evil-paste-after--from-copy-register (&optional count)
  "So often do we paste from the copy register, let's make it a command and bind it to a key and forget about \"0p"
  (interactive "p")
  (evil-paste-after (or count 1) ?0))

;;;###autoload
(defun my/evil-paste-before--from-copy-register (&optional count)
  "So often do we paste from the copy register, let's make it a command and bind it to a key and forget about \"0p"
  (interactive "p")
  (evil-paste-before (or count 1) ?0))
