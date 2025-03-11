;;; init-eaf.el --- insert description here -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package eaf
  :commands (eaf-open eaf-open-git eaf-open-browser
                      eaf-open-file-manager eaf-open-in-file-manager
                      eaf-open-mind-elixir
                      eaf-markdown-previewer-open)
  :custom
  (eaf-config-location (concat my-cache-dir "eaf"))
  :init
  (map! (:map embark-file-map
         :desc "Open with eaf"     "e"    #'eaf-open)))

(use-package eaf-git
  :after eaf)
(use-package eaf-file-manager
  :custom
  (eaf-file-manager-show-hidden-file nil)
  :after eaf)
(use-package eaf-pdf-viewer
  :after eaf
  :config
  (map! :map eaf-pdf-outline-mode-map
        :gn "RET"      #'eaf-pdf-outline-jump))
(use-package eaf-browser
  :after eaf)
(use-package eaf-markdown-previewer
  :after eaf)
(use-package eaf-org-previewer
  :after eaf)
(use-package eaf-mind-elixir
  :after eaf)
(use-package eaf-evil
  :after eaf
  :config
  (add-hook! 'evil-normal-state-entry-hook
    (defun my-eaf-map-a()
      (map! :map eaf-mode-map
            :gn "C-d"      #'eaf-py-proxy-scroll_down_page
            :gn "C-u"      #'eaf-py-proxy-scroll_up_page
            :gn "<next>"   #'eaf-py-proxy-scroll_up_page
            :gn "<prior>"  #'eaf-py-proxy-scroll_down_page
            :gn "q"        (cmds! (commandp 'eaf-py-proxy-close_buffer)
                                  'eaf-py-proxy-close_buffer)
            :gn "M-o"      #'ace-window
            :gn "M-j"      #'xsort-tab-ace-jump
            :gn "M-h"      #'xsort-tab-select-prev-tab
            :gn "M-l"      #'xsort-tab-select-next-tab)))
  (setq eaf-evil-leader-keymap my-leader-map)
  (setq eaf-evil-leader-key "SPC"))

(provide 'init-eaf)
