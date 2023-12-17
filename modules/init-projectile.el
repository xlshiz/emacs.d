;;; init-projectile.el --- projectile config. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar my-projectile-cache-limit 10000
  "If any project cache surpasses this many files it is purged when quitting
Emacs.")

(defvar my-projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(defvar my-projectile-cache-purge-non-projects nil
  "If non-nil, non-projects are purged from the cache on `kill-emacs-hook'.")

(defvar my-projects--fd-version nil)

(use-package projectile
  :diminish projectile-mode "ⓟ"
  :hook (after-init . projectile-mode)
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :config
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes 
        '(".dir" ".cmake" ".make" ".internal" ".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/")
        projectile-ignored-project-function #'my-project-ignored-p)

  ;; HACK: Projectile cleans up the known projects list at startup. If this list
  ;;   contains tramp paths, the `file-remote-p' calls will pull in tramp via
  ;;   its `file-name-handler-alist' entry, which is expensive. Since Doom
  ;;   already cleans up the project list on kill-emacs-hook, it's simplest to
  ;;   inhibit this cleanup process at startup (see bbatsov/projectile#1649).
  (letf! ((#'projectile--cleanup-known-projects #'ignore))
    (projectile-mode +1))
  ;; HACK: Auto-discovery and cleanup on `projectile-mode' is slow and
  ;;   premature. Let's try to defer it until it's needed.
  (add-transient-hook! 'projectile-relevant-known-projects
    (projectile--cleanup-known-projects)
    (when projectile-auto-discover
      (projectile-discover-projects-in-search-path)))

  ;; Projectile runs four functions to determine the root (in this order):
  ;;
  ;; + `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;    for an explicit path.
  ;; + `projectile-root-bottom-up' -> searches from / to your current directory
  ;;   for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;   includes .git and .project
  ;; + `projectile-root-top-down' -> searches from the current directory down to
  ;;   / the paths listed in `projectile-root-files', like package.json,
  ;;   setup.py, or Cargo.toml
  ;; + `projectile-root-top-down-recurring' -> searches from the current
  ;;   directory down to / for a directory that has one of
  ;;   `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;   parent directory with the same file.
  ;;
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".project"     ; project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  (add-to-list 'projectile-globally-ignored-directories "build")
  (push (abbreviate-file-name my-cache-dir) projectile-globally-ignored-directories)
  (push (abbreviate-file-name (concat user-emacs-directory "etc")) projectile-globally-ignored-directories)
  (push (abbreviate-file-name (concat user-emacs-directory "lib")) projectile-globally-ignored-directories)

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  ;; Support the more generic .project files as an alternative to .projectile
  (defadvice! my--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (cond ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
          ((expand-file-name ".project" (projectile-project-root)))))

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  ;; Treat current directory in dired as a "file in a project" and track it
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook! 'kill-emacs-hook
    (defun my-cleanup-project-cache-h ()
      "Purge projectile cache entries that:

a) have too many files (see `my-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `my-projectile-cache-blacklist'),
c) are not valid projectile projects."
      (when (and (bound-and-true-p projectile-projects-cache)
                 projectile-enable-caching)
        (setq projectile-known-projects
              (cl-remove-if #'projectile-ignored-project-p
                            projectile-known-projects))
        (projectile-cleanup-known-projects)
        (cl-loop with blacklist = (mapcar #'file-truename my-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (string-empty-p proot)
                        (>= (length (gethash proot projectile-projects-cache))
                            my-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (and my-projectile-cache-purge-non-projects
                             (not (my-project-p proot)))
                        (projectile-ignored-project-p proot))
                 do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache))))

  ;; HACK Don't rely on VCS-specific commands to generate our file lists. That's
  ;;      7 commands to maintain, versus the more generic, reliable and
  ;;      performant `fd' or `ripgrep'.
  (defadvice! my--only-use-generic-command-a (fn vcs)
    "Only use `projectile-generic-command' for indexing project files.
And if it's a function, evaluate it."
    :around #'projectile-get-ext-command
    (if (and (functionp projectile-generic-command)
             (not (file-remote-p default-directory)))
        (funcall projectile-generic-command vcs)
      (let ((projectile-git-submodule-command
             (get 'projectile-git-submodule-command 'initial-value)))
        (funcall fn vcs))))

  ;; `projectile-generic-command' doesn't typically support a function, but my
  ;; `my--only-use-generic-command-a' advice allows this. I do it this way so
  ;; that projectile can adapt to remote systems (over TRAMP), rather then look
  ;; for fd/ripgrep on the remote system simply because it exists on the host.
  ;; It's faster too.
  ;; (put 'projectile-git-submodule-command 'initial-value projectile-git-submodule-command)
  (setq projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid
        projectile-generic-command
        (lambda (_)
          ;; If fd exists, use it for git and generic projects. fd is a rust
          ;; program that is significantly faster than git ls-files or find, and
          ;; it respects .gitignore. This is recommended in the projectile docs.
          (cond
           ((when-let*
                ((bin (if (ignore-errors (file-remote-p default-directory nil t))
                          (cl-find-if (my-rpartial #'executable-find t)
                                      (list "fdfind" "fd"))
                        my-fd-binary))
                 ;; REVIEW Temporary fix for #6618. Improve me later.
                 (version (with-memoization my-projects--fd-version
                            (cadr (split-string (cdr (my-call-process bin "--version"))
                                                " " t))))
                 ((ignore-errors (version-to-list version))))
                (concat (format "%s . -0 -H --color=never --type file --type symlink --follow --exclude .git %s"
                                bin (if (version< version "8.3.0")
                                        "" "--strip-cwd-prefix"))
                        (if is-windows-p " --path-separator=/"))))
           ;; Otherwise, resort to ripgrep, which is also faster than find
           ((executable-find "rg" t)
            (concat "rg -0 --files --follow --color=never --hidden -g!.git"
                    (if is-windows-p " --path-separator=/")))
           ("find . -type f -print0"))))

  (defadvice! my--projectile-default-generic-command-a (fn &rest args)
    "If projectile can't tell what kind of project you're in, it issues an error
when using many of projectile's command, e.g. `projectile-compile-command',
`projectile-run-project', `projectile-test-project', and
`projectile-configure-project', for instance.

This suppresses the error so these commands will still run, but prompt you for
the command instead."
    :around #'projectile-default-generic-command
    (ignore-errors (apply fn args))))

(provide 'init-projectile)
;;; init-projectile ends here
