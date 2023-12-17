;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(when (version< emacs-version "28.1")
  (error "Your Emacs is too old -- this config requires 28.1 or higher"))

;; Speed up startup
(setq auto-mode-case-fold nil)
(setq site-run-file nil)

;; Optimize loading performance
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-file-name-handler-alist)))))))

;; Optimize emacs garbage collect.
(setq gc-cons-threshold most-positive-fixnum)

;; Hook run after loading init files
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

;; Suppress flashing at startup
(setq-default inhibit-message t
              inhibit-redisplay t)
(add-hook 'window-setup-hook (lambda ()
                               (setq-default inhibit-message nil
                                             inhibit-redisplay nil)
                               (redisplay)))
(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; Load core config of emacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)
(my/initialize-core)

;; Be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Test packages
(add-to-list 'load-path (expand-file-name "lib/wgrep" user-emacs-directory))
;; Editor
(require 'init-evil)
(require 'init-ui)
(require 'init-highlight)
(require 'init-edit)
(require 'init-window)
(require 'init-undo)
(require 'init-minibuffer)
(require 'init-completion)
;; Tools
(require 'init-projectile)
(require 'init-vc)
(require 'init-term)
(require 'init-dired)
(require 'init-snippet)
(require 'init-lsp)
(require 'init-format)
(require 'init-treesit)
(require 'init-chinese)
(when (file-exists-p (expand-file-name "eaf" my-etc-dir))
  (add-to-list 'load-path (expand-file-name "eaf" my-etc-dir))
  (require 'init-eaf))
;; Langs
(require 'init-prog)
(require 'init-elisp)
(require 'init-org)
(require 'init-markdown)
(require 'init-cc)
(require 'init-golang)
(require 'init-python)
(require 'init-rust)
(require 'init-data)
(require 'init-lua)
(require 'init-sh)

;; Keybindings
(require 'init-keybindings.el)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
