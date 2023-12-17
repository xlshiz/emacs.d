;; init-lua.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package lua-mode
  :mode "\\.lua$"
  :init
  (setq lua-indent-level 2))

(provide 'init-lua)
;;; init-prog.el ends here
