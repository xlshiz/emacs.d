;;; core-package.el --- package install config. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Install use-package
(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)
(setq byte-compile-warnings '(cl-functions))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package autoload
  :init
  (defun my/generate-autoload-define (loaddef &rest DIRS)
    (interactive)
    (let ((generated-autoload-file loaddef))
      (when (not (file-exists-p loaddef))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";; generated by function: `my/generate-autoload-define'.")
          (save-buffer)))
      (dolist (d DIRS)
        (apply 'update-directory-autoloads (list d))
        (dolist (it (directory-files d t "^[^.]" t))
          (when (file-directory-p it)
            (apply 'update-directory-autoloads (list it))))))))

(when (not (file-exists-p my-autoload-file))
  (my/generate-autoload-define my-autoload-file (concat user-emacs-directory "core/autoload/"))
  (my/generate-autoload-define my-autoload-file (concat user-emacs-directory "modules/autoload/"))
  (byte-compile-file my-autoload-file)
  (message "generate autoload file: %s done." my-autoload-file))
(load my-autoload-file nil 'nomessage)

(defvar my--deferred-packages-alist '(t))
(with-eval-after-load 'use-package-core
  ;; `use-package' adds syntax highlighting for the `use-package' macro, but
  ;; Emacs 26+ already highlights macros, so it's redundant.
  (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  ;; We define :minor and :magic-minor from the `auto-minor-mode' package here
  ;; so we don't have to load `auto-minor-mode' so early.
  (dolist (keyword '(:minor :magic-minor))
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :commands)))

  (defalias 'use-package-normalize/:minor #'use-package-normalize-mode)
  (defun use-package-handler/:minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-alist arg rest state))

  (defalias 'use-package-normalize/:magic-minor #'use-package-normalize-mode)
  (defun use-package-handler/:magic-minor (name _ arg rest state)
    (use-package-handle-mode name 'auto-minor-mode-magic-alist arg rest state))

  ;; HACK Fix `:load-path' so it resolves relative paths to the containing file,
  ;;      rather than `user-emacs-directory'. This is a done as a convenience
  ;;      for users, wanting to specify a local directory.
  (defadvice! my--resolve-load-path-from-containg-file-a (fn label arg &optional recursed)
    "Resolve :load-path from the current directory."
    :around #'use-package-normalize-paths
    ;; `use-package-normalize-paths' resolves paths relative to
    ;; `user-emacs-directory', so we change that.
    (let ((user-emacs-directory
           (or (and (stringp arg)
                    (not (file-name-absolute-p arg))
                    (ignore-errors (dir!)))
               user-emacs-directory)))
      (funcall fn label arg recursed)))

  ;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
  ;;
  ;;   :after-call SYMBOL|LIST
  ;;   :defer-incrementally SYMBOL|LIST|t
  ;;
  ;; Check out `use-package!'s documentation for more about these two.
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((my-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "my--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   ;; (message "use-package: lazy loading %s from %s" ',name ',fn)
                   (condition-case e
                       ;; If `default-directory' is a directory that doesn't
                       ;; exist or is unreadable, Emacs throws up file-missing
                       ;; errors, so we set it to a directory we know exists and
                       ;; is readable.
                       (let ((default-directory user-emacs-directory))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name my--deferred-packages-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (delq deferral-list my--deferred-packages-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name my--deferred-packages-alist)
             (push '(,name) my--deferred-packages-alist))
           (nconc (assq ',name my--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "epkgs/" my-cache-dir))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))


(use-package benchmark-init
  :demand t
  :config
  (require 'benchmark-init-modes)
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(use-package simple
  :hook ((after-init . size-indication-mode) ; 显示百分比进度
         (text-mode . visual-line-mode))
  :init
  (setq line-number-mode t              ; 打开行号显示
        column-number-mode t            ; 打开列号显示
        kill-whole-line nil               ; Kill line including '\n'
        line-move-visual nil            ; Move line by visual line
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        column-number-indicator-zero-based nil ; column starts from 1
        kill-do-not-save-duplicates t)   ; eliminate duplicates
  ;; 设置visual line fringe bitmap
  (when (and (fboundp 'define-fringe-bitmap) (display-graphic-p))
    (define-fringe-bitmap 'right-curly-arrow
      [#b00000000
       #b01111100
       #b01111100
       #b00001100
       #b00001100
       #b00000000
       #b00000000])
    (define-fringe-bitmap 'left-curly-arrow
      [#b00000000
       #b00110000
       #b00110000
       #b00111110
       #b00111110
       #b00000000
       #b00000000])
    (set-fringe-bitmap-face 'right-curly-arrow 'warning)
    (set-fringe-bitmap-face 'left-curly-arrow 'warning)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))))

(use-package so-long
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 10240)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; disable some mode that may be unnecessary/expensive for large buffer
  (add-to-list 'so-long-minor-modes 'rainbow-mode)
  (add-to-list 'so-long-minor-modes 'flycheck-mode)
  (add-to-list 'so-long-minor-modes 'eldoc-mode)
  (add-to-list 'so-long-minor-modes 'ws-butler-mode)
  (add-to-list 'so-long-minor-modes 'undo-tree-mode)
  (add-to-list 'so-long-minor-modes 'highlight-numbers-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'highlight-indent-guides-mode))

(use-package hydra)
(use-package popup)
(use-package posframe)
(use-package diminish)

;; Buffer index
(use-package imenu
  :hook (imenu-after-jump . recenter))

;; Don't litter emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name my-etc-dir)
        no-littering-var-directory (expand-file-name my-cache-dir))
  (setq custom-file (expand-file-name "custom.el" my-local-dir)))

(use-package better-jumper
  :hook (my-first-input . better-jumper-mode)
  :commands my-set-jump-a my-set-jump-maybe-a my-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward] #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  ;; xref jump
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (setq better-jumper-context 'window
        better-jumper-new-window-behavior 'copy
        better-jumper-add-jump-behavior 'replace
        better-jumper-max-length 100
        better-jumper-use-evil-jump-advice t)

  (defun my-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun my-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun my-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)
  ;; Auto set mark cmd list
  (dolist (cmd '(lsp-bridge-find-def kill-current-buffer imenu citre-jump))
    (advice-add cmd :around #'my-set-jump-a)))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package gcmh
  :hook (my-first-input . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(add-hook 'my-after-init-hook #'my-load-packages-incrementally-h 100)

(provide 'core-package)
;;; core-package.el ends here
