;;; init-project.el --- projectile config. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

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
        ;; v2.9+: stale known projects are cleaned lazily on access, never at
        ;; startup, and remote projects are kept without probing.
        projectile-auto-cleanup-known-projects t
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes
        '(".dir" ".cmake" ".make" ".internal" ".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/")
        projectile-ignored-project-function #'+project-ignored-p)

  ;; Trim projectile's marker lists: root resolution is faster when a file has
  ;; no project (it must search every candidate marker).
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".project"     ; project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; Populated by other modules; keep it minimal here so projectile does
        ;; fewer file checks when resolving a root.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  ;; Never index build output or local generated directories
  (dolist (dir (list "build"
                     (abbreviate-file-name my-cache-dir)
                     (abbreviate-file-name (concat user-emacs-directory "etc"))
                     (abbreviate-file-name (concat user-emacs-directory "lib"))))
    (add-to-list 'projectile-globally-ignored-directories dir))

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  ;; Support the more generic .project files as an alternative to .projectile
  (defadvice! my--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (or (file-exists-p! (or ".projectile" ".project") (projectile-project-root))
        (expand-file-name ".project" (projectile-project-root))))

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  ;; v2.8+ indexes git projects with fd natively (`projectile-git-use-fd',
  ;; with per-host detection on TRAMP). Keep the old single-command semantics:
  ;; hidden files and followed symlinks are included for git and generic
  ;; projects alike, and fd falls back to find when missing.
  (setq projectile-indexing-method 'hybrid
        projectile-git-fd-args
        "-H -0 -E .git --type file --type symlink --follow --strip-cwd-prefix -c never"
        projectile-generic-command
        (if-let* ((fd projectile-fd-executable))
            (concat fd
                    " . -0 -H --color=never --type file --type symlink"
                    " --follow --exclude .git --strip-cwd-prefix")
          "find . -type f -print0"))

  (defadvice! my--projectile-default-generic-command-a (fn &rest args)
    "If projectile can't tell what kind of project you're in, it issues an error
when using many of projectile's command, e.g. `projectile-compile-command',
`projectile-run-project', `projectile-test-project', and
`projectile-configure-project', for instance.

This suppresses the error so these commands will still run, but prompt you for
the command instead."
    :around #'projectile-default-generic-command
    (ignore-errors (apply fn args))))

(provide 'init-project)
;;; init-project.el ends here
