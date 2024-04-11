;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(pcase my-lsp-backend
  ('lsp-bridge
   (use-package lsp-bridge
     :defer t
     :init
     (add-hook 'lsp-bridge-mode-hook #'evil-normalize-keymaps)
     :config
     (setq lsp-bridge-enable-diagnostics t
           lsp-bridge-enable-log nil)
     (map! (:map acm-mode-map
           [C-return]    (cmd! (acm-hide)
                               (newline-and-indent))))
     (map! :map lsp-bridge-mode-map
       :n    "ga"  #'xref-find-apropos
       :n    "gd"  #'lsp-bridge-find-def
       :n    "gi"  #'lsp-bridge-find-impl
       :n    "gr"  #'lsp-bridge-find-references
       :n    "ge"  #'lsp-bridge-diagnostic-list
       :n    "gt"  #'lsp-bridge-find-type-def
       :n    "gj"  #'lsp-bridge-diagnostic-jump-next
       :n    "gk"  #'lsp-bridge-diagnostic-jump-prev
       :n    "K"   #'lsp-bridge-popup-documentation
       :n    "C-t" #'lsp-bridge-find-def-return
       :n    "gR"  #'lsp-bridge-rename)))
  ('lsp-mode
   (defvar +lsp-defer-shutdown 3
     "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")

   (defvar +lsp--default-read-process-output-max nil)
   (defvar +lsp--default-gcmh-high-cons-threshold nil)
   (defvar +lsp--optimization-init-p nil)

   (define-minor-mode +lsp-optimization-mode
     "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
     :global t
     :init-value nil
     (if (not +lsp-optimization-mode)
         (setq-default read-process-output-max +lsp--default-read-process-output-max
                       gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                       +lsp--optimization-init-p nil)
       ;; Only apply these settings once!
       (unless +lsp--optimization-init-p
         (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
               +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
         (setq-default read-process-output-max (* 1024 1024))
         ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
         ;;        library, so we up the GC threshold to stave off GC-induced
         ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
         ;;        so we modify its variables rather than `gc-cons-threshold'
         ;;        directly.
         (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
         (gcmh-set-high-threshold)
         (setq +lsp--optimization-init-p t))))
   (use-package lsp-mode
     :defer t
     :commands lsp-install-server
     :diminish lsp-mode
     :hook (lsp-mode . lsp-enable-which-key-integration)
     :init
     ;; Performace tuning
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
     (setq read-process-output-max (* 1024 1024)) ;; 1MB
     (setq lsp-keep-workspace-alive nil
           lsp-response-timeout 5
           lsp-session-file (concat my-cache-dir "lsp-session")
           lsp-server-install-dir (concat my-etc-dir "lsp")

           lsp-go-links-in-hover nil

           lsp-headerline-breadcrumb-enable nil

           lsp-eldoc-render-all nil
           lsp-eldoc-enable-hover nil

           lsp-signature-auto-activate nil
           lsp-signature-render-documentation nil

           lsp-modeline-diagnostics-enable nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-workspace-status-enable nil

           lsp-enable-folding nil
           lsp-enable-indentation nil
           lsp-enable-file-watchers nil
           lsp-enable-on-type-formatting nil
           lsp-enable-text-document-color nil
           lsp-enable-symbol-highlighting nil)

     ;; For `lsp-clients'
     (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     :config
     (setq lsp-intelephense-storage-path (concat my-etc-dir "lsp-intelephense/")
           lsp-vetur-global-snippets-dir
           (expand-file-name "vetur" (concat user-emacs-directory "snippets/"))
           lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
           lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

     ;; REVIEW Remove this once this is fixed upstream.
     (add-to-list 'lsp-client-packages 'lsp-racket)
     (defadvice! +lsp--respect-user-defined-checkers-a (fn &rest args)
       "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
       :around #'lsp-diagnostics-flycheck-enable
       (if flycheck-checker
           (let ((old-checker flycheck-checker))
             (apply fn args)
             (setq-local flycheck-checker old-checker))
         (apply fn args)))

     (add-hook! 'lsp-mode-hook #'+lsp-optimization-mode)
     (defvar +lsp--deferred-shutdown-timer nil)
     (defadvice! +lsp-defer-server-shutdown-a (fn &optional restart)
       "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
       :around #'lsp--shutdown-workspace
       (if (or lsp-keep-workspace-alive
               restart
               (null +lsp-defer-shutdown)
               (= +lsp-defer-shutdown 0))
           (prog1 (funcall fn restart)
             (+lsp-optimization-mode -1))
         (when (timerp +lsp--deferred-shutdown-timer)
           (cancel-timer +lsp--deferred-shutdown-timer))
         (setq +lsp--deferred-shutdown-timer
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda (workspace)
                      (with-lsp-workspace workspace
                        (unless (lsp--workspace-buffers workspace)
                          (let ((lsp-restart 'ignore))
                            (funcall fn))
                          (+lsp-optimization-mode -1))))
                lsp--cur-workspace))))

     (custom-set-faces
      '(lsp-face-highlight-textual ((t (:background "honeydew" :bold t))))
      '(lsp-face-highlight-read ((t (:background "honeydew" :bold t))))
      '(lsp-face-highlight-write ((t (:background "beige" :bold t)))))

     (map! :map lsp-mode-map
       :n    "ga" #'xref-find-apropos
       :n    "gd" #'lsp-find-definition
       :n    "gi" #'lsp-find-implementation
       :n    "gr" #'lsp-find-references
       :n    "ge" #'lsp-treemacs-errors-list
       :n    "gt" #'lsp-find-type-definition
       :n    "gh" #'lsp-treemacs-call-hierarchy)
     ;; (my/local-leader-define
     ;;   "="  '(:ignore t :wk "formatting")
     ;;   "=b" 'lsp-format-buffer
     ;;   "=r" 'lsp-format-region
     ;;   "c"  '(:ignore t :wk "code")
     ;;   "ca" 'lsp-execute-code-action
     ;;   "ch" 'lsp-document-highlight
     ;;   "cl" 'lsp-avy-lens
     ;;   "g"  '(:ignore t :wk "goto")
     ;;   "ga" 'xref-find-apropos
     ;;   "gd" 'lsp-find-definition
     ;;   "gD" 'lsp-find-declaration
     ;;   "ge" 'lsp-treemacs-errors-list
     ;;   "gh" 'lsp-treemacs-call-hierarchy
     ;;   "gr" 'lsp-find-references
     ;;   "gt" 'lsp-find-type-definition
     ;;   "p"  '(:ignore t :wk "peek")
     ;;   "pg" 'lsp-ui-peek-find-definitions
     ;;   "pi" 'lsp-ui-peek-find-implementation
     ;;   "pr" 'lsp-ui-peek-find-references
     ;;   "ps" 'lsp-ui-peek-find-workspace-symbol
     ;;   "h"  '(:ignore t :wk "help")
     ;;   "hg" 'lsp-ui-doc-glance
     ;;   "hh" 'lsp-describe-thing-at-point
     ;;   "hs" 'lsp-signature-activate
     ;;   "i"  '(:ignore t :wk "import")
     ;;   "r"  '(:ignore t :wk "refactor")
     ;;   "rr" 'lsp-rename
     ;;   "t"  '(:ignore t :wk "toggle")
     ;;   "w"  '(:ignore t :wk "workspace")
     ;;   "wa" 'lsp-workspace-folders-add
     ;;   "wd" 'lsp-describe-session
     ;;   "wq" 'lsp-workspace-shutdown
     ;;   "wr" 'lsp-workspace-restart)

     (defun my/lsp--init-if-visible (func &rest args)
       "Not enabling lsp in `git-timemachine-mode'."
       (unless (bound-and-true-p git-timemachine-mode)
         (apply func args)))
     (advice-add #'lsp--init-if-visible :around #'my/lsp--init-if-visible)

     (use-package lsp-ui
       :custom-face
       (lsp-ui-sideline-code-action ((t (:inherit warning))))
       :bind (:map lsp-ui-mode-map
                   ([remap evil-goto-definition] . lsp-ui-peek-find-definitions)
                   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                   ([remap xref-find-references] . lsp-ui-peek-find-references))
       :hook (lsp-mode . lsp-ui-mode)
       :init
       (setq lsp-ui-doc-enable (display-graphic-p)
             lsp-ui-doc-delay 0.5
             lsp-ui-doc-include-signature nil
             lsp-ui-doc-position 'at-point
             lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
             lsp-ui-doc-use-webkit nil

             lsp-ui-sideline-show-hover nil
             lsp-ui-sideline-show-diagnostics nil
             lsp-ui-sideline-ignore-duplicate t
             lsp-ui-sideline-show-code-actions nil

             lsp-ui-imenu-enable t
             lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                   ,(face-foreground 'font-lock-string-face)
                                   ,(face-foreground 'font-lock-constant-face)
                                   ,(face-foreground 'font-lock-variable-name-face)))
       :config
       ;; HACK: lsp-ui-doc frame background color when use doom-one theme
       (add-to-list 'lsp-ui-doc-frame-parameters '(background-color . "#2e3138"))
       ;; (add-to-list 'lsp-ui-doc-frame-parameters '(background-color . "#313131"))
       ;; Reset `lsp-ui-doc' after loading theme
       (add-hook 'after-load-theme-hook
                 (lambda ()
                   (setq lsp-ui-doc-border (face-background 'posframe-border nil t))))

       ;; `C-g'to close doc
       (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

     ;; Python: pyright
     (use-package lsp-pyright
       :hook (python-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred)))
       :init
       (when (executable-find "python3")
         (setq lsp-pyright-python-executable-cmd "python3")))

     (use-package consult-lsp
       :defer t
       :init
       (map! :map lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
