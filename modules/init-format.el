;;; init-format.el --- code format config. -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;
;;; Hacks

(add-hook!
 'apheleia-post-format-hook
 ;; HACK `web-mode' doesn't update syntax highlighting after arbitrary buffer
 ;;      modifications, so we must trigger refontification manually.
 (defun +format--fix-web-mode-fontification-h ()
   (when (eq major-mode 'web-mode)
     (setq web-mode-fontification-off nil)
     (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
       (save-excursion
         (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end))))))


(provide 'init-format)
;;; init-format.el ends here
