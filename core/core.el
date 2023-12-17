;;; core.el --- emacs 核心配置. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;   - hook: `before-init-hook'
;;   - hook: `after-init-hook'
;;   - hook: `emacs-startup-hook'
;;   - hook: `window-setup-hook'
;;   - hook: `my-load-theme-hook'
;;   - hook: `my-after-init-hook'
;;   > After startup is complete:
;;     - On first input:              `my-first-input-hook'
;;
;;; Code:

;;;;;;;;;
;;; HOOKS
(defcustom my-load-theme-hook ()
  "Hook run after the theme is loaded with `load-theme'."
  :group 'my
  :type 'hook)
(unless noninteractive
  (define-advice load-theme (:after (&rest _) my-load-theme-h)
    (run-hooks 'my-load-theme-hook))
  (add-hook 'my-load-theme-hook #'window-divider-mode))

(defcustom my-after-init-hook ()
  "A hook run once config are loaded.
This triggers at the absolute latest point in the eager startup process, and
runs in both interactive and non-interactive sessions, so guard hooks
appropriately against `noninteractive' or the `cli' context."
  :group 'my
  :type 'hook)
(unless noninteractive
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (run-hooks 'my-after-init-hook)))

(defcustom my-first-input-hook ()
  "Transient hooks run before the first user input."
  :group 'my
  :local 'permanent-local
  :type 'hook)
(add-hook 'pre-command-hook #'(lambda ()
                                (when my-first-input-hook
                                  (run-hooks 'my-first-input-hook)
                                  (setq my-first-input-hook nil))))


;;;;;;;
;; Ensure core dir is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(add-to-list 'load-path (concat user-emacs-directory "modules/langs"))

(defun my/initialize-core ()
  "Load core config file for Emacs."
  (require 'core-defvar)
  (require 'core-basic)
  (require 'core-package)
  (require 'core-keybinds))


(provide 'core)
;;; core.el ends here
