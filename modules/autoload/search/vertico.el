;;; vertico.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +project-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (+project-project-root dir)
       t))

;;;###autoload
(defun +project-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun +project-ignored-p (project-root)
  "Return non-nil if temporary file or a straight package."
  (unless (file-remote-p project-root)
    (or (file-in-directory-p project-root temporary-file-directory)
        (file-in-directory-p project-root (concat user-emacs-directory "lib")))))

;;;###autoload
(cl-defun +consult--grep (&key query in all-files args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current directory
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended."
  (declare (indent defun))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((directory (or in default-directory))
         (consult-ripgrep-args +vertico-rg-args)
         (query (or query
                    (when (my-region-active-p)
                      (regexp-quote (my-thing-at-point-or-region)))))
         (consult-async-split-styles-alist
          (copy-sequence consult-async-split-styles-alist)))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key separator initial function)
          (alist-get consult-async-split-style consult-async-split-styles-alist)
        ;; Perl async split style starts with an #. If the query contains #,
        ;; then use oneof the alternative delimiters instead.
        (if (eq consult-async-split-style 'perl)
            (when (string-match-p (char-to-string initial) query)
              (setf (alist-get 'perlalt consult-async-split-styles-alist)
                    `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                             unless (string-match-p char query)
                                             return char)
                                    "%")
                      :separator ,separator
                      :function ,function)
                    consult-async-split-style 'perlalt))
          ;; If the separator character is present *in* the query, escape them.
          (when separator
            (setq query
                  (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                            (concat "\\" (char-to-string separator))
                                            query t t))))))
    (let ((consult-preview-key "C-."))
      (consult--grep "Grep" #'consult--ripgrep-make-builder directory query))))

;;;###autoload
(defun +consult/grep-project (&optional all initial-query)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (let ((project-root (or (+project-project-root) default-directory)))
    (+consult--grep :query initial-query :in project-root :all-files all)))

;;;###autoload
(defun +consult/grep-cwd (&optional all initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+consult--grep :query initial-query :in default-directory :all-files all))

;;;###autoload
(defun +consult/grep-dir (&optional all initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (let ((default-directory
         (read-directory-name "Search directory: ")))
    (+consult--grep :query initial-query :in default-directory :all-files all)))

;;;###autoload
(defun +consult/grep-another-project (&optional all initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (let ((default-directory
         (if-let* ((projects (projectile-relevant-known-projects)))
             (completing-read "Search project: " projects nil t)
           (user-error "There are no known projects"))))
    (+consult--grep :query initial-query :in default-directory :all-files all)))

;;;###autoload
(defun +consult/grep-symbol-in-buffer ()
  "Performs a search in the current buffer for thing at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +consult/grep-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (if (and start end (not multiline-p))
          (consult-line
           (replace-regexp-in-string
            " " "\\\\ "
            (rxt-quote-pcre
             (buffer-substring-no-properties start end))))
        (call-interactively #'consult-line)))))

;;;###autoload
(defun +consult/grep-symbol-in-project (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (my-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let* ((projects (projectile-relevant-known-projects)))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (+project-project-root default-directory)))))
  (+consult--grep :query symbol :in dir))

(defvar +consult-find-file-in--history nil)
;;;###autoload
(cl-defun +consult--fd (&key (async t) dir (initial ""))
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (declare (indent defun))
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (consult-fd-args +vertico-fd-args)
         (builder (consult--fd-make-builder '(".")))
         (cmd (split-string-and-unquote +vertico-fd-args " "))
         (consult-preview-key "C-."))
    (if async
        (find-file (consult--find (car (consult--directory-prompt "Fd" default-directory)) builder initial))
      (find-file
       (consult--read
        (split-string (cdr (apply #'my-call-process cmd)) "\n" t)
        :prompt default-directory
        :sort nil
        :initial (unless (string-empty-p initial) (shell-quote-argument initial))
        :add-history (thing-at-point 'filename)
        :category 'file
        :history '(:input +consult-find-file-in--history))))))

;;;###autoload
(defun +consult/search-file-cwd ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (+consult--fd :async nil :dir default-directory))

;;;###autoload
(defun +consult/search-file-project ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (let ((project-root (or (+project-project-root) default-directory)))
    (+consult--fd :async t :dir project-root)))

;;;###autoload
(defun +embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;;;###autoload
(defun +consult/jump-list (jump)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive
   (let (buffers)
     (require 'consult)
     (unwind-protect
         (list
          (consult--read
           ;; REVIEW Refactor me
           (nreverse
            (delete-dups
             (delq
              nil (mapcar
                   (lambda (mark)
                     (when mark
                       (cl-destructuring-bind (path pt _id) mark
                         (let* ((visiting (find-buffer-visiting path))
                                (buf (or visiting (find-file-noselect path t)))
                                (dir default-directory))
                           (unless visiting
                             (push buf buffers))
                           (with-current-buffer buf
                             (goto-char pt)
                             (font-lock-fontify-region
                              (line-beginning-position) (line-end-position))
                             (format "%s:%d: %s"
                                     (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                         (file-relative-name (buffer-file-name buf) dir))
                                                   #'< :key #'length))
                                     (line-number-at-pos)
                                     (string-trim-right (or (thing-at-point 'line) ""))))))))
                   (cddr (better-jumper-jump-list-struct-ring
                          (better-jumper-get-jumps (better-jumper--get-current-context))))))))
           :prompt "jumplist: "
           :sort nil
           :require-match t
           :category 'jump-list))
       (mapc #'kill-buffer buffers))))
  (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
      (user-error "No match")
    (let ((file (match-string-no-properties 1 jump))
          (line (match-string-no-properties 2 jump)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line)))))

(autoload #'consult--directory-prompt "consult")
;;;###autoload
(defun +consult/buffer ()
  (interactive)
  (let ((selected (consult--read
                   (consult--buffer-query :sort 'visibility
                                          :as #'buffer-name)
                   :require-match (confirm-nonexistent-file-or-buffer)
                   :prompt "Switch to: "
                   :category 'buffer)))
    (unless (plist-get selected :match)
      (consult--buffer-action selected))))
