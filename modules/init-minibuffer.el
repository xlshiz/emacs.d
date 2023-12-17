;;; init-minibuffer.el --- minibuffer config. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

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
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
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
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (advice-add #'marginalia--project-root :override #'my-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
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
  (defvar +vertico-consult-fd-args nil
    "Shell command and arguments the vertico module uses for fd.")

  :config
  (setq consult-line-numbers-widen t
        consult-narrow-key "<"
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-project-root-function #'projectile-project-root)
  (unless +vertico-consult-fd-args
    (setq +vertico-consult-fd-args
          (if my-fd-binary
              (format "%s --color=never -i -H -E .git --regex %s"
                      my-fd-binary
                      (if is-windows-p "--path-separator=/" ""))
            consult-find-args)))
  (consult-customize  consult-bookmark consult-recent-file
                      consult-xref consult-buffer
                      consult-ripgrep consult-git-grep consult-grep
                      :preview-key "C-."
                      consult-bookmark consult-theme
                      :preview-key '(:debounce 0.5 any))
  (consult-customize
    my/consult-ripgrep-at-point
    snail--source-buffer snail--source-project-file snail--source-recent-file snail--source-hidden-buffer
    +embark-find-file +embark-find-file-cwd +embark-find-file-other-dir +embark-find-file-other-project
    +embark/grep-project +embark-grep-other-cwd +embark-grep-other-project
    +default/search-project +default/search-other-project
    +default/search-project-for-symbol-at-point
    +default/search-cwd +default/search-other-cwd
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
          ("C-c C-e" . +vertico/embark-export-write))
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
                 "." #'+embark-find-file
                 "f" #'+embark-find-file-cwd
                 "F" #'+embark-find-file-other-dir
                 "P" #'+embark-find-file-other-project
                 "A" #'snail)
  (add-to-list 'embark-become-keymaps '+embark-become-snail-map)

  (defvar-keymap +embark-become-grep-map
                 :doc "Keymap for Embark become."
                 :parent nil
                 "p" #'+embark/grep-project
                 "P" #'+embark-grep-other-project
                 "G" #'+embark-grep-other-cwd
                 "g" #'+default/search-project-for-symbol-at-point
                 "b" #'+embark-grep-buffer)
  (add-to-list 'embark-become-keymaps '+embark-become-grep-map))

(use-package embark-consult
  :after embark consult)

;; Writable `grep' buffer
(use-package wgrep
  :hook (grep-setup . wgrep-setup)
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;; HACK: Filter boring message in echo area.
(defadvice message (around my-message-filter activate)
  (unless (string-match "gofmt\\|skipped\\|tsc-dyn-get" (or (ad-get-arg 0) ""))
    ad-do-it))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
