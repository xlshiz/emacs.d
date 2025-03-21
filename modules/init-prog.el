;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook #'xref-after-jump-hook #'recenter)
  (add-hook #'xref-after-jump-hook #'better-jumper-set-jump)
  (add-hook #'xref-after-return-hook #'recenter)
  (add-hook #'xref-after-return-hook #'better-jumper-set-jump))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))


(use-package citre
  :init
  (setq
   citre-project-root-function #'projectile-project-root
   citre-default-create-tags-file-location 'project-cache
   citre-use-project-root-when-creating-tags nil
   ;; citre-peek-fill-fringe nil
   citre-prompt-language-for-ctags-command t)
  :config
  (require 'citre-config)
  ;; HACK!!
  ;; (add-hook 'citre-peek--mode-hook #'evil-normalize-keymaps)
  (map! :n "g C-]"  #'citre-peek
        :n "C-]"    #'citre-jump)
  (map! (:map citre-peek-keymap
         "M-n"     #'citre-peek-next-line
         "M-p"     #'citre-peek-prev-line
         :n [right]   #'citre-peek-chain-forward
         :n [left]    #'citre-peek-chain-backward
         "M-m p"   #'citre-peek-through)))
  ;; (general-evil-define-key '(normal motion) citre-peek-keymap
  ;;        "M-n"     #'citre-peek-next-line
  ;;        "M-p"     #'citre-peek-prev-line
  ;;        "M-N"     #'citre-peek-next-definition
  ;;        "M-P"     #'citre-peek-prev-definition
  ;;        [right]   #'citre-peek-chain-forward
  ;;        [left]    #'citre-peek-chain-backward
  ;;        [up]      #'citre-peek-prev-branch
  ;;        [down]    #'citre-peek-next-branch
  ;;        "M-m p"   #'citre-peek-through
  ;;        "M-m d"   #'citre-peek-delete-branch
  ;;        "M-m D"   #'citre-peek-delete-branches
  ;;        [S-up]    #'citre-peek-move-current-def-up
  ;;        [S-down]  #'citre-peek-move-current-def-down
  ;;        "M-m f"   #'citre-peek-make-current-def-first
  ;;        "M-m j"   #'citre-peek-jump
  ;;        "q"       #'citre-peek-abort))

(use-package symbols-outline
  :commands (symbols-outline-smart-toggle)
  :init
  (setq symbols-outline-follow-symbol-in-origin nil)
  :config
  ;; HACK!!
  (evil-make-overriding-map symbols-outline-mode-map 'normal)
  (map! :map symbols-outline-mode-map
       :n "g"       #'symbols-outline-refresh
       :n "j"       #'symbols-outline-next
       :n "k"       #'symbols-outline-prev
       :n "TAB"     #'symbols-outline-toggle-node
       :n "RET"     #'symbols-outline-visit
       :n "M-RET"   #'symbols-outline-visit-and-quit
       :n "q"       #'quit-window)
  (setq symbols-outline-window-position 'right)
  (symbols-outline-follow-mode))

(use-package flycheck
  :defer t
  :commands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  (map! :map flycheck-error-list-mode-map
        :n "C-n"    #'flycheck-error-list-next-error
        :n "C-p"    #'flycheck-error-list-previous-error
        :n "j"      #'flycheck-error-list-next-error
        :n "k"      #'flycheck-error-list-previous-error
        :n "RET"    #'flycheck-error-list-goto-error
        :n [return] #'flycheck-error-list-goto-error))

(use-package aidermacs
  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password :host "openrouter.ai"))
  (setq aidermacs-backend 'vterm)
  :custom
  (aidermacs-show-diff-after-change t)
  (aidermacs-use-architect-mode t)
  (aidermacs-architect-model "openrouter/deepseek/deepseek-r1:free")
  (aidermacs-editor-model "openrouter/deepseek/deepseek-chat:free")
  (aidermacs-default-model "openrouter/deepseek/deepseek-r1:free"))

(provide 'init-prog)
;;; init-prog.el ends here
