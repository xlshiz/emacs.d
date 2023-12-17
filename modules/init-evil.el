;;; init-evil.el --- setup emacs use evil keybinds. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package evil
  :demand t ; https://github.com/noctuid/general.el/issues/180
  :hook (after-init . evil-mode)
  :preface
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        evil-mode-line-format nil
        evil-want-C-g-bindings t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-disable-insert-state-bindings t ; enable default emacs keybinding in insert state
        evil-undo-system 'undo-fu
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; move back the cursor one position when exiting insert mode
        evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
        evil-esc-delay 0.01)
  ;; cursor appearance
  (setq evil-default-cursor '(box (lambda () (evil-set-cursor-color my/default-cursor-color)))
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(bar (lambda () (evil-set-cursor-color my/emacs-cursor-color)))
        evil-insert-state-cursor '(bar . 2)
        evil-visual-state-cursor 'hollow)
  ;; Fix #7141
  (defadvice! +evil--persist-state-a (fn &rest args)
              "When changing major modes, Evil's state is lost. This advice preserves it."
              :around #'set-auto-mode
              (if evil-state
                (evil-save-state (apply fn args))
                (apply fn args)))

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
              evil-ex-hl-update-delay 0.25)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must.
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  ;; --- keybind fixes ----------------------
  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  ;; --- evil hacks -------------------------
  (after! eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (my-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  ;; HACK '=' moves the cursor to the beginning of selection. Disable this,
  ;;      since it's more disruptive than helpful.
  (defadvice! +evil--dont-move-cursor-a (fn &rest args)
    :around #'evil-indent
    (save-excursion (apply fn args)))

  ;; REVIEW In evil, registers 2-9 are buffer-local. In vim, they're global,
  ;;        so... Perhaps this should be PRed upstream?
  (defadvice! +evil--make-numbered-markers-global-a (char)
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  ;; REVIEW Fix #2493: dir-locals cannot target fundamental-mode when evil-mode
  ;;        is active. See hlissner/doom-emacs#2493. Revert this if
  ;;        emacs-evil/evil#1268 is resolved upstream.
  (defadvice! +evil--fix-local-vars-a (&rest _)
    :before #'turn-on-evil-mode
    (when (eq major-mode 'fundamental-mode)
      (hack-local-variables)))

  ;; HACK Invoking helpful from evil-ex throws a "No recursive edit is in
  ;;      progress" error because, between evil-ex and helpful,
  ;;      `abort-recursive-edit' gets called one time too many.
  (defadvice! +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
    :before #'helpful-key
    (when (evil-ex-p)
      (run-at-time 0.1 nil #'helpful-key key-sequence)
      (abort-recursive-edit)))

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply fn args)))

  ;; completion state map
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
  ;; normal state map
  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map ";w" #'save-buffer)
  (define-key evil-normal-state-map ";j" #'avy-goto-char-3)
  ;; visual state map
  (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)

  ;; Specify major mode uses EMACS original state.
  (dolist (p '((minibuffer-inactive-mode . emacs)
               (calendar-mode . emacs)
               (anaconda-nav-mode . emacs)
               (log-edit-mode . emacs)
               (vc-log-edit-mode . emacs)
               (magit-log-edit-mode . emacs)
               (lsp-bridge-ref-mode . emacs)
               (profiler-report-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p)))

  ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar my/default-cursor-color "#ffffff")
  (defvar my/emacs-cursor-color "#ff9999")
  (add-hook 'my-load-theme-hook (lambda ()
                                  (setq my/default-cursor-color (face-background 'cursor)
                                        my/emacs-cursor-color (face-foreground 'warning))))

  (use-package evil-escape
    :diminish evil-escape-mode
    :hook (evil-mode . evil-escape-mode)
    :config
    (setq evil-escape-delay 0.25
          evil-escape-key-sequence ",."
          evil-escape-excluded-major-modes '(neotree-mode)
          evil-escape-excluded-states '(normal visual multiedit emacs motion))
    ;; no `evil-escape' in minibuffer
    (add-hook 'evil-escape-inhibit-functions #'minibufferp))

  (use-package evil-surround
    :hook (evil-mode . global-evil-surround-mode))

  (use-package evil-nerd-commenter
    :bind ([remap comment-dwim] . evilnc-comment-or-uncomment-lines)
    :init
    (evil-define-key '(normal visual) global-map
      (kbd "gcc") 'evilnc-comment-or-uncomment-lines
      (kbd "gcp") 'evilnc-comment-or-uncomment-paragraphs))

  (use-package evil-collection
    :after evil
    :config
    ;; The list of supported modes is configured by evil-collection-mode-list
    (evil-collection-init 'view)
    (evil-collection-init 'magit)
    (evil-collection-init 'custom)
    (evil-collection-init 'ibuffer)
    (evil-collection-init 'calendar)
    (evil-collection-init 'flycheck)
    (evil-collection-init 'vterm))

  (use-package evil-terminal-cursor-changer
    :unless (display-graphic-p)
    :config
    ;; cursor appearance in terminal
    (setq evil-default-cursor '(box (lambda () (evil-set-cursor-color my/default-cursor-color)))
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  '(hbar (lambda () (evil-set-cursor-color my/emacs-cursor-color)))
          evil-insert-state-cursor '(bar . 2)
          evil-visual-state-cursor 'hollow)
    (evil-terminal-cursor-changer-activate))

  ;; s: 2 char forward; S: 2 char backward
  ;; f: 1 char forward; F: 1 char backward
  ;; ;and, repeat search
  (use-package evil-snipe
    :hook ((evil-mode . evil-snipe-mode)
           (evil-mode . evil-snipe-override-mode))
    :diminish evil-snipe-local-mode
    :init
    (setq evil-snipe-smart-case t
          evil-snipe-scope 'line
          evil-snipe-repeat-scope 'visible
          evil-snipe-char-fold t)
    :config
    (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq))

  (use-package evil-multiedit
    :config
    (evil-multiedit-default-keybinds))

  (use-package evil-textobj-anyblock
    :defer t
    :config
    (setq evil-textobj-anyblock-blocks
          '(("(" . ")")
            ("{" . "}")
            ("\\[" . "\\]")
            ("<" . ">"))))

  ;; Allows you to use the selection for * and #
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search
               evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (evil-define-key* 'visual 'global
      "*" #'evil-visualstar/begin-search-forward
      "#" #'evil-visualstar/begin-search-backward))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode))

  (use-package evil-org
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(provide 'init-evil)
;;; init-evil ends here
