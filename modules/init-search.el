;;; init-search.el --- search config. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
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
        projectile-frecency-file (concat my-cache-dir "projectile-frecency.eld")
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")
        ;; v2.9+: stale known projects are cleaned lazily on access, never at
        ;; startup, and remote projects are kept without probing.
        projectile-auto-cleanup-known-projects t
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes
        '(".dir" ".cmake" ".make" ".internal" ".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
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

  ;; Never index build output.  These entries are gitignore-style patterns
  ;; matched against paths *relative to the project root*, so the abbreviated
  ;; absolute paths this used to hold (`~/.emacs.d/lib' and friends) could
  ;; never match anything; those dirs are excluded nowhere now (an anchored
  ;; pattern such as "/lib" here, or a dirconfig file, would do it).
  (add-to-list 'projectile-globally-ignored-directories "build")

  ;; `fd' walks submodule working trees as part of the superproject listing
  ;; (`projectile-git-use-fd'), so projectile's own per-submodule listings are
  ;; pure duplication: in this repo (188 submodules) they spent ~12s spawning
  ;; one lister per submodule and returned no file the superproject listing
  ;; didn't already contain.  Keep them when fd isn't driving the listing -
  ;; `git ls-files' stops at submodule boundaries, so then they are the only
  ;; way submodule files are seen at all.
  (setq projectile-git-submodule-command
        (unless projectile-git-use-fd projectile-git-submodule-command))

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +consult/grep-project instead")
  (put 'projectile-ripgrep 'disabled "Use +consult/grep-project instead")
  (put 'projectile-grep 'disabled "Use +consult/grep-project instead")

  ;; v2.8+ indexes git projects with fd natively (`projectile-git-use-fd',
  ;; with per-host detection on TRAMP). Keep the old single-command semantics:
  ;; hidden files and followed symlinks are included for git and generic
  ;; projects alike, and fd falls back to find when missing.
  ;; Not `alien': its fd `--exclude' patterns carry a trailing slash that fd
  ;; never matches, and the Lisp filtering is skipped in the same breath.
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

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-j"   . vertico-next-group)
              ("M-k"   . vertico-previous-group)
              ("C-j"   . vertico-next)
              ("C-k"   . vertico-previous)
              ("M-o"   . ace-window)
              ("M-RET" . +search/consult-to-color-rg))
  :init
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle nil)
  (setq enable-recursive-minibuffers t)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Quick action
  (use-package vertico-quick
    :after vertico
    :ensure nil
    :bind (:map vertico-map
                ([tab] . vertico-directory-enter)
                ("C-i" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))
  ;; Repeat last session
  (use-package vertico-repeat
    :after vertico
    :ensure nil
    :bind ("C-c C-r" . vertico-repeat)
    :config
    (add-hook! 'minibuffer-setup-hook #'vertico-repeat-save))
  ;;  Ido-like directory navigation
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-w" . vertico-directory-delete-word))
    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (add-hook! 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (advice-add #'marginalia--project-root :override #'+project-project-root)
  (pushnew! marginalia-command-categories
            '(+consult/search-file-cwd . file)
            '(flycheck-error-list-set-filter . builtin)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after nerd-icons marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :hook (my-first-input . savehist-mode)
  :init
  (setq history-length 200
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables '(kill-ring                         ; persist clipboard
                                        register-alist                    ; persist macros
                                        mark-ring global-mark-ring        ; persist marks
                                        search-ring regexp-search-ring))) ; persist searches

(use-package consult
  :bind (([remap isearch-forward]               . consult-line)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop))
  :commands (consult--read)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (defvar +vertico-fd-args
    (if my-fd-binary
        (format "%s --color=never -i -H -E .git --regex %s"
                my-fd-binary
                (if is-windows-p "--path-separator=/" ""))
      consult-find-args)
    "Shell command and arguments the vertico module uses for fd.")
  (defvar +vertico-rg-args
    (if my-rg-binary
        (format "%s %s "
                my-rg-binary
                (concat
                 "--null --line-buffered --color=never --max-columns=1000 "
                 "--path-separator /   --smart-case --no-heading "
                 "--with-filename --line-number --search-zip "
                 "--hidden -g !.git -g !.svn -g !.hg "))
      consult-grep-args)
    "Shell command and arguments the vertico module uses for rg.")
  :config
  (setq consult-line-numbers-widen t
        consult-narrow-key "<"
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-project-root-function #'projectile-project-root)
  (consult-customize  consult-bookmark consult-recent-file
                      consult-xref consult-buffer
                      consult-ripgrep consult-git-grep consult-grep
                      :preview-key "C-."
                      consult-bookmark consult-theme
                      :preview-key '(:debounce 0.5 any))
  (consult-customize
    snail--source-buffer snail--source-project-file snail--source-recent-file snail--source-hidden-buffer
    +embark-find-file +embark-find-file-cwd +embark-find-file-other-dir +embark-find-file-other-project
    +embark-grep-other-dir +embark-grep-other-project
    +consult/grep-symbol-in-project
    :preview-key "C-.")

  (use-package consult-dir
    :bind (([remap list-directory] . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))

(use-package embark
  :bind  (([remap describe-bindings] . embark-bindings)
          ("M-a" . embark-act)
          ("M-." . embark-become)
          ("C-c C-;" . embark-export)
          ("C-c C-l" . embark-collect)
          ("C-c C-e" . +embark-export-write))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  ;; From the embark wiki
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t))))

  ;; Embark indicators
  (setq embark-indicators '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
          (window-height . (lambda (win) (fit-window-to-buffer
                                          win (floor (frame-height)
                                                     3))))))
  (defadvice! +embark-become-command-a (fn &rest args)
              :around #'embark--become-command
              (let ((command (cl-first args))
                    (use-dialog-box nil)
                    (input (cl-second args)))
                (if (equal (substring (format "%s" command) 0 8) "+embark-")
                  (funcall command input)
                  (apply fn args))))
  (defvar-keymap +embark-become-snail-map
                 :doc "Keymap for Embark become."
                 :parent nil
                 "A" #'snail
                 "." #'+embark-find-file
                 "f" #'+embark-find-file-cwd
                 "d" #'+embark-find-file-other-dir
                 "p" #'+embark-find-file-other-project)
  (add-to-list 'embark-become-keymaps '+embark-become-snail-map)

  (defvar-keymap +embark-become-grep-map
                 :doc "Keymap for Embark become."
                 :parent nil
                 "P" #'+consult/grep-project
                 "G" #'+consult/grep-symbol-in-project
                 "B" #'+consult/grep-buffer
                 "p" #'+embark-grep-other-project
                 "d" #'+embark-grep-other-dir
                 "b" #'+embark-grep-buffer)
  (add-to-list 'embark-become-keymaps '+embark-become-grep-map))

(use-package embark-consult
  :after embark consult)

;; Writable `grep' buffer
(use-package wgrep
  :hook (grep-setup . wgrep-setup)
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package color-rg
  :commands (color-rg-search-input color-rg-search-symbol
              color-rg-search-symbol-in-current-file color-rg-search-project)
  :init
  (defconst evil-collection-color-rg-maps '(color-rg-mode-map
                                             color-rg-mode-edit-map))
  (after! evil-collection
    (+evil-collection-color-rg-setup))
  :config
  (advice-add #'color-rg-update-header-line :override #'ignore)
  (defhydra color-rg-hydra (:hint nil)
    "
    ^^^^Move               ^^^^filter                     ^^toggle            ^^change
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
    _n_   next keyword   | _r_   replace all            | _I_  toggle ignore  | _d_  change dir
    _p_   prev keyword   | _f_   filter match result    | _c_  toggle case    | _z_  change globs
    _N_   next file      | _F_   filter mismatch result | _i_  open edit mode | _Z_  change exclude
    _P_   prev file      | _x_   filter match files     | ^^                  | _t_  return literal
    _D_   remove line    | _X_   filter mismatch files  | _u_  unfilter       | _s_  return regexp
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
  "
    ("n" color-rg-jump-next-keyword)
    ("p" color-rg-jump-prev-keyword)
    ("N" color-rg-jump-next-file)
    ("P" color-rg-jump-prev-file)

    ("r" color-rg-replace-all-matches)
    ("f" color-rg-filter-match-results)
    ("F" color-rg-filter-mismatch-results)
    ("x" color-rg-filter-match-files)
    ("X" color-rg-mismatch-files)
    ("u" color-rg-unfilter)
    ("D" color-rg-remove-line-from-results)

    ("I" color-rg-rerun-toggle-ignore)
    ("t" color-rg-rerun-literal)
    ("c" color-rg-rerun-toggle-case)
    ("s" color-rg-rerun-regexp)
    ("d" color-rg-rerun-change-dir)
    ("z" color-rg-rerun-change-globs)
    ("Z" color-rg-rerun-change-exclude-files)
    ("C" color-rg-customized-search)
    ("i" color-rg-switch-to-edit-mode)
    ("q" nil "quit")))

;; HACK: Filter boring message in echo area.
(defadvice! my-message-filter-a (orig-fun &rest args)
  :around #'message
  (unless (string-match "gofmt\\|skipped\\|tsc-dyn-get" (or (car args) ""))
    (apply orig-fun args)))

(provide 'init-search)
;;; init-minibuffer.el ends here
