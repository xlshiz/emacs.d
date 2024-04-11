;;; init-completion.el --- completion config. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enhance completion at point.
(use-package corfu
  :hook (my-first-input . global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-count 15)
  (corfu-bar-width 0.5)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-max-width 100)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("C-n" . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("C-p" . corfu-previous)
              ("C-j" . corfu-insert)
              ("C-g" . corfu-quit))
  :config
  (setq corfu-excluded-modes '(shell-mode
                               eshell-mode
                               comint-mode
                               erc-mode
                               gud-mode
                               rcirc-mode
                               text-mode
                               minibuffer-inactive-mode))
  (advice-add #'keyboard-quit :before #'corfu-quit)
  (after! evil
    ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
    (advice-add 'corfu--setup :after (lambda (&rest _) (evil-normalize-keymaps)))
    (advice-add 'corfu--teardown :after (lambda (&rest _) (evil-normalize-keymaps)))
    (evil-make-overriding-map corfu-map)
    ;; auto quit corfu when exit insert state
    (add-hook 'evil-normal-state-entry-hook (lambda ()
                                              (when corfu--candidates (corfu-quit)))))

  ;; extensions
  (use-package corfu-quick
    :after corfu
    :bind (:map corfu-map
                ("C-q" . corfu-quick-insert)))
  (use-package corfu-history
    :after corfu
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :after corfu
    :hook (corfu-mode . corfu-popupinfo-mode)
    :bind (:map corfu-map
                ("K" . corfu-popupinfo-toggle)))

  (use-package corfu-english-helper
    :after cape
    :defer t)

  ;; A bunch of completion at point extensions
  (use-package cape
    :after corfu
    :bind (("C-c p p" . completion-at-point) ;; capf
           ("C-c p t" . complete-tag)        ;; etags
           ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
           ("C-c p h" . cape-history)
           ("C-c p f" . cape-file)
           ("C-c p k" . cape-keyword)
           ("C-c p s" . cape-elisp-symbol)
           ("C-c p e" . cape-elisp-block)
           ("C-c p a" . cape-abbrev)
           ("C-c p l" . cape-line)
           ("C-c p w" . cape-dict)
           ("C-c p :" . cape-emoji))
    :init
    ;; 默认补全后端
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file))

  (defun my/set-mixed-capf ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions (list
                                               (cape-capf-buster
                                                (cape-super-capf
                                                 (pcase my-lsp-backend
                                                   ('lsp-bridge #'lsp-bridge-capf)
                                                   ('lsp-mode #'lsp-completion-at-point))
                                                 #'cape-file
                                                 #'cape-keyword
                                                 #'cape-dabbrev)
                                                'equal))))

  ;; (add-hook 'lsp-bridge-mode-hook #'my/set-mixed-capf)
  (add-hook 'lsp-completion-mode-hook #'my/set-mixed-capf))

(provide 'init-completion)
;;; init-completion.el ends here
