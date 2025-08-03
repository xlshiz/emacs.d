;;; init-ui.el ---  setup ui. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package solaire-mode)

(use-package doom-themes
  :init
  (add-hook 'my-load-theme-hook #'doom-themes-org-config)
  (add-hook 'my-load-theme-hook #'doom-themes-neotree-config)
  (add-hook 'my-load-theme-hook #'doom-themes-visual-bell-config)
  (setq doom-dark+-blue-modeline t
        doom-gruvbox-dark-variant "medium"
        doom-themes-neotree-file-icons t
        doom-themes-neotree-line-spacing 2))

;; 延迟部分ui设置
(add-hook 'my-after-init-hook
          (defun my-ui-setup-h ()
            ;; 加载doom主题依赖
            (solaire-global-mode)
            ;; 加载主题
            (if (daemonp)
                (add-hook 'after-make-frame-functions (lambda (frame) (load-theme 'doom-gruvbox t)))
              (load-theme 'doom-nord-light t))

            (when (display-graphic-p)
              ;; Frame maximized
              (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
              (add-to-list 'default-frame-alist '(fullscreen . maximized))

              ;; Specify default font
              (cl-loop for font in '("Sarasa Fixed SC" "M PLUS Code Latin 50")
                       when (font-installed-p font)
                       return (set-face-attribute 'default nil
                                                  :font font
                                                  :height 150))
              ;; Specify font for all unicode characters
              (cl-loop for font in '("Symbola")
                       when (font-installed-p font)
                       return(set-fontset-font t 'unicode font nil 'prepend))
              ;; Specify font for Chinese characters
              (cl-loop for font in '("Sarasa Fixed SC" "Source Han Mono SC" "Microsoft Yahei")
                       when (font-installed-p font)
                       return (dolist (charset '(kana han cjk-misc bopomofo))
                                (set-fontset-font (frame-parameter nil 'font)
                                                  charset
                                                  font))))))

(use-package vi-tilde-fringe
  :if (fboundp 'set-fringe-mode)
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

(use-package nerd-icons
  :if (display-graphic-p)
  :config
  (setq nerd-icons-scale-factor 0.95))

(use-package awesome-tray
  :init
  (setq awesome-tray-minibuffer nil)
  :config
  (add-hook 'my-load-theme-hook  #'awesome-tray-mode)
  ;; (awesome-tray-mode 1)
  (defun my--location-info ()
    (format "%s:%s"
            (format-mode-line "%l")
            (format-mode-line "%c")
            ))
  (add-to-list 'awesome-tray-module-alist
               '("loc" . (my--location-info awesome-tray-module-date-face)))
  (defun my--lsp-info ()
    (with-demoted-errors
        ""
      (cond ((and (featurep 'lsp-mode) (functionp 'lsp-workspaces) (lsp-workspaces))
             "lsp")
            ((and (featurep 'lsp-bridge) lsp-bridge-mode)
             "lsp+")
            (t ""))))
  (add-to-list 'awesome-tray-module-alist
               '("lsp+" . (my--lsp-info awesome-tray-module-circe-face)))
  (setq awesome-tray-active-modules
        '("file-path" "loc" "mode-name" "lsp+" "git" "evil")))

(use-package xsort-tab
  :defer 0
  :custom-face
  (xsort-tab-ace-keys-face ((t :inherit hydra-face-red :bold t :height 1.0)))
  :config
  (window-divider-mode -1)
  (setq uniquify-buffer-name-style 'forward)
  (defhydra tabs-fast-switch (:hint nil)
    "
    ^^^^Tab                    ^^Misc
   -^^^^---------------------+-^^^^---------------------------
    _C-a_^^     select first | _C-k_^^   close tab
    _C-e_^^     select last  | _C-j_^^   ace jump
    _h_   _l_  switch tab    | _C-S-k_  close other tabs
   -^^^^---------------------+-^^^^---------------------------
  "
    ("h" xsort-tab-select-prev-tab)
    ("l" xsort-tab-select-next-tab)
    ("C-a" xsort-tab-select-first-tab)
    ("C-e" xsort-tab-select-last-tab)
    ("C-k" xsort-tab-close-current-tab)
    ("C-S-k" xsort-tab-close-other-tabs)
    ("C-j" xsort-tab-ace-jump)
    ("q" nil "quit"))
  (setq xsort-tab-height 20)
  (setq xsort-tab-hide-buffers (append '("COMMIT_EDITMSG" "dir-data-")
                                 xsort-tab-hide-buffers))
  (setq xsort-tab-hide-tabs (append '(" *snails" "COMMIT_EDITMSG" " *rime" "*color-rg*" "*vterm-popup:main*" "*Ilist*" "*Outline*")
                              xsort-tab-hide-tabs))
  (xsort-tab-mode t))


(provide 'init-ui)
;;; init-ui ends here
