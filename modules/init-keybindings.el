;;; init-keybindings.el --- insert description here -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-y") 'yank-pop)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up   1)))

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;;;
;; Modes
(define-key! help-map
  ;; new keybinds
  "'"    #'describe-char
  "w"    #'+default/man-at-point
  "C-k"  #'describe-key-briefly
  "C-l"  #'describe-language-environment
  "C-m"  #'info-emacs-manual

  ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
  ;; <leader> h prefix. It's already on ? and F1 anyway.
  "C-h"  nil

  ;; make `describe-bindings' available under the b prefix which it previously
  ;; occupied. Add more binding related commands under that prefix as well
  "b"    nil
  "bb"   #'describe-bindings
  "bi"   #'which-key-show-minor-mode-keymap
  "bm"   #'which-key-show-major-mode
  "bt"   #'which-key-show-top-level
  "bf"   #'which-key-show-full-keymap
  "bk"   #'which-key-show-keymap

  ;; replaces `apropos-command'
  "a"    #'apropos
  "A"    #'apropos-documentation
  ;; replaces `describe-copying' b/c not useful
  "C-c"  #'describe-coding-system
  ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
  "F"    #'describe-face
  ;; replaces `view-hello-file' b/c annoying
  "h"    nil
  ;; replaces `help-with-tutorial', b/c it's less useful than `load-theme'
  "t"    #'load-theme
  ;; replaces `describe-package' b/c redundant w/ `doom/help-packages'
  "P"    #'find-library)


(map! :gni  "M-s"      #'save-buffer)

;;; win&tab
(map! :gni  "M-o"      #'ace-window
      :ni "M-j"        #'xsort-tab-ace-jump
      :ni "M-h"        #'xsort-tab-select-prev-tab
      :ni "M-l"        #'xsort-tab-select-next-tab
      (:after info
       :map Info-mode-map
       :ni "M-o"       #'ace-window))

;;; term
(map! :ni   "M-t"      #'+vterm/toggle
      :ni   [f5]       #'+vterm/toggle)

;;; avy-thing-edit
(map! (:prefix-map ("M-y" . "avy-copy-and-yank")
       :i "w"      #'avy-thing-copy-and-yank-word
       :i "o"      #'avy-thing-copy-and-yank-symbol
       :i "x"      #'avy-thing-copy-and-yank-sexp
       :i "l"      #'avy-thing-copy-and-yank-line
       :i "b"      #'avy-thing-copy-and-yank-parentheses
       :i "("      #'avy-thing-copy-and-yank-parentheses
       :i "p"      #'avy-thing-copy-and-yank-paragraph
       :i "{"      #'avy-thing-copy-and-yank-paragraph
       :i "n"      #'avy-thing-copy-and-yank-number
       :i "f"      #'avy-thing-copy-and-yank-defun
       :i "e"      #'avy-thing-copy-and-yank-email
       :i "i"      #'avy-thing-copy-and-yank-filename
       :i "t"      #'avy-thing-copy-and-yank-list
       :i "u"      #'avy-thing-copy-and-yank-url)
      (:prefix-map ("C-c j" . "avy-thing-edit")
       (:prefix-map ("c" . "copy")
        :ni "w"      #'avy-thing-copy-word
        :ni "o"      #'avy-thing-copy-symbol
        :ni "x"      #'avy-thing-copy-sexp
        :ni "l"      #'avy-thing-copy-line
        :ni "b"      #'avy-thing-copy-parentheses
        :ni "("      #'avy-thing-copy-parentheses
        :ni "p"      #'avy-thing-copy-paragraph
        :ni "{"      #'avy-thing-copy-paragraph
        :ni "n"      #'avy-thing-copy-number
        :ni "f"      #'avy-thing-copy-defun
        :ni "e"      #'avy-thing-copy-email
        :ni "i"      #'avy-thing-copy-filename
        :ni "t"      #'avy-thing-copy-list
        :ni "u"      #'avy-thing-copy-url)
       (:prefix-map ("y" . "copy and yank")
        :i "w"      #'avy-thing-copy-and-yank-word
        :i "o"      #'avy-thing-copy-and-yank-symbol
        :i "x"      #'avy-thing-copy-and-yank-sexp
        :i "l"      #'avy-thing-copy-and-yank-line
        :i "b"      #'avy-thing-copy-and-yank-parentheses
        :i "("      #'avy-thing-copy-and-yank-parentheses
        :i "p"      #'avy-thing-copy-and-yank-paragraph
        :i "{"      #'avy-thing-copy-and-yank-paragraph
        :i "n"      #'avy-thing-copy-and-yank-number
        :i "f"      #'avy-thing-copy-and-yank-defun
        :i "e"      #'avy-thing-copy-and-yank-email
        :i "i"      #'avy-thing-copy-and-yank-filename
        :i "t"      #'avy-thing-copy-and-yank-list
        :i "u"      #'avy-thing-copy-and-yank-url)
       (:prefix-map ("x" . "cut")
        :ni "w"      #'avy-thing-cut-word
        :ni "o"      #'avy-thing-cut-symbol
        :ni "x"      #'avy-thing-cut-sexp
        :ni "l"      #'avy-thing-cut-line
        :ni "b"      #'avy-thing-cut-parentheses
        :ni "("      #'avy-thing-cut-parentheses
        :ni "p"      #'avy-thing-cut-paragraph
        :ni "{"      #'avy-thing-cut-paragraph
        :ni "n"      #'avy-thing-cut-number
        :ni "f"      #'avy-thing-cut-defun
        :ni "e"      #'avy-thing-cut-email
        :ni "i"      #'avy-thing-cut-filename
        :ni "t"      #'avy-thing-cut-list
        :ni "u"      #'avy-thing-cut-url)
       (:prefix-map ("r" . "replace")
        :ni "w"      #'avy-thing-replace-word
        :ni "o"      #'avy-thing-replace-symbol
        :ni "x"      #'avy-thing-replace-sexp
        :ni "l"      #'avy-thing-replace-line
        :ni "b"      #'avy-thing-replace-parentheses
        :ni "("      #'avy-thing-replace-parentheses
        :ni "p"      #'avy-thing-replace-paragraph
        :ni "{"      #'avy-thing-replace-paragraph
        :ni "n"      #'avy-thing-replace-number
        :ni "f"      #'avy-thing-replace-defun
        :ni "e"      #'avy-thing-replace-email
        :ni "i"      #'avy-thing-replace-filename
        :ni "t"      #'avy-thing-replace-list
        :ni "u"      #'avy-thing-replace-url)))

(map! :i [C-tab]     #'completion-at-point
      (:prefix-map ("M-p" . "cape")
       :i "e"        #'corfu-english-helper-search
       :i "d"        #'cape-dabbrev
       :i "h"        #'cape-history
       :i "f"        #'cape-file
       :i "k"        #'cape-keyword
       :i "s"        #'cape-elisp-symbol
       :i "S"        #'consult-yasnippet
       :i "a"        #'cape-abbrev
       :i "l"        #'cape-line
       :i "w"        #'cape-dict))

;; Smart key
(map! :i [tab]      (cmds! (yas-maybe-expand-abbrev-key-filter 'yas-expand)
                           #'yas-expand)
      :gi   "C-a"   #'my/backward-to-bol-or-indent
      :gi   "C-e"   #'my/forward-to-last-non-comment-or-eol)

;; evil key
(map! :m "]t"   #'hl-todo-next
      :m "[t"   #'hl-todo-previous
      ;; evil-surround
      :v "S"    #'evil-surround-region
      :o "s"    #'evil-surround-edit
      :o "S"    #'evil-Surround-edit
      :nv "zp"  #'my/evil-paste-after--from-copy-register
      :nv "zP"  #'my/evil-paste-before--from-copy-register
      ;; text objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "c" #'evilnc-inner-comment              #'evilnc-outer-commenter
      :textobj "f" #'+evil:defun-txtobj                #'+evil:defun-txtobj
      :textobj "g" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "q" #'+evil:inner-any-quote             #'+evil:outer-any-quote
      :textobj "u" #'+evil:inner-url-txtobj            #'+evil:outer-url-txtobj
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr)

(map! :leader
      :desc "Find file"                    "."    #'find-file
      :desc "Switch to last buffer"        "`"    #'evil-switch-to-windows-last-buffer
      :desc "Run terminal"                 "'"    #'vterm
      :desc "Search symbol in project"     "*"    #'+default/search-project-for-symbol-at-point
      :desc "Search project"               "/"    #'+embark/grep-project

      :desc "M-x"                          "SPC"  #'execute-extended-command
      :desc "Jump to bookmark"             "RET"  #'bookmark-jump
      :desc "Alternate buffer"             "TAB"  #'my/alternate-buffer-in-persp

      :desc "Consult Find"                 "a"    #'snail
      :desc "Dired"                        "d"    #'dired-jump
      :desc "eaf file manager"             "D"    #'eaf-open-in-file-manager
      :desc "eaf git client"               "G"    #'eaf-open-git
      :desc "help"                         "h"    help-map
      :desc "Resume last search"           "r"    #'vertico-repeat
      :desc "Universal argument"           "u"    #'universal-argument
      :desc "Org Capture"                  "x"    #'org-capture

      ;;; <leader> b --- buffer
      (:prefix-map  ("b" . "buffer")
       :desc "Alternate buffer"            "TAB"  #'my/alternate-buffer-in-persp
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       :desc "Switch buffer"               "A"   #'eaf-kill-process
       :desc "Switch buffer"               "b"   #'+vertico/buffer
       :desc "Clone buffer"                "c"   #'clone-indirect-buffer
       :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
       :desc "Kill tab buffer"             "d"   #'xsort-tab-close-current-tab-and-select-previous
       :desc "Sort-tab kill tab buffer"    "k"   #'xsort-tab-close-current-tab
       :desc "Kill mode buffers"           "K"   #'xsort-tab-close-mode-tabs
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "Kill other buffers"          "O"   #'my/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Rename buffer"               "R"   #'rename-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       ;; :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       ;; :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       ;; :desc "Find implementations"                  "i"   #'+lookup/implementations
       ;; :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       ;; :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       ;; :desc "Find type definition"                  "t"   #'+lookup/type-definition
       ;; :desc "List errors"                           "x"   #'+default/diagnostics
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'my/delete-trailing-newlines)

      ; <leader> f --- file
      (:prefix-map ("f" . "file")
       :desc "Find file"                   "."   #'find-file
       :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
       :desc "Copy this file"              "C"   #'my/copy-file
       :desc "Find directory"              "d"   #'+default/dired
       ;; :desc "Delete this file"            "D"   #'doom/delete-this-file
       ;; :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
       ;; :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
       :desc "Recursive find file"         "f"   #'+default/find-file-under-here
       :desc "Find file from here"         "F"   #'+default/find-file-under-here
       :desc "Locate file"                 "l"   #'locate
       ;; :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
       ;; :desc "Browse private config"       "P"   #'doom/open-private-config
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Rename/move file"            "R"   #'my/rename-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       ;; :desc "Sudo find file"              "u"   #'doom/sudo-find-file
       ;; :desc "Sudo this file"              "U"   #'doom/sudo-this-file
       :desc "Yank file path"              "y"   #'+default/yank-buffer-path
       :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project)

      ; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Revert file"               "R"   #'vc-revert
       :desc "SMerge"                    "m"   #'hydra-smerge-mode/body
       :desc "Git stage hunk"            "s"   #'hydra-diff-hl/body
       :desc "Git time machine"          "t"   #'git-timemachine-toggle
       :desc "Magit dispatch"            "/"   #'magit-dispatch
       :desc "Magit file dispatch"       "."   #'magit-file-dispatch
       :desc "Forge dispatch"            "'"   #'forge-dispatch
       :desc "Magit switch branch"       "b"   #'magit-branch-checkout
       :desc "Magit status"              "g"   #'magit-status
       :desc "Magit status here"         "G"   #'magit-status-here
       :desc "Magit diff"                "d"   #'magit-diff
       :desc "Magit file delete"         "D"   #'magit-file-delete
       :desc "Magit blame"               "B"   #'magit-blame-addition
       :desc "Magit clone"               "C"   #'magit-clone
       :desc "Magit fetch"               "F"   #'magit-fetch
       :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
       :desc "Git stage file"            "S"   #'magit-stage-file
       :desc "Git unstage file"          "U"   #'magit-unstage-file
       (:prefix ("f" . "find")
        :desc "Find file"                "f"   #'magit-find-file
        :desc "Find gitconfig file"      "g"   #'magit-find-git-config-file
        :desc "Find commit"              "c"   #'magit-show-commit
        :desc "Find issue"               "i"   #'forge-visit-issue
        :desc "Find pull request"        "p"   #'forge-visit-pullreq)
       (:prefix ("o" . "open in browser")
        :desc "Browse file or region"    "o"   #'+vc/browse-at-remote
        :desc "Browse homepage"          "h"   #'+vc/browse-at-remote-homepage
        :desc "Browse remote"            "r"   #'forge-browse-remote
        :desc "Browse commit"            "c"   #'forge-browse-commit
        :desc "Browse an issue"          "i"   #'forge-browse-issue
        :desc "Browse a pull request"    "p"   #'forge-browse-pullreq
        :desc "Browse issues"            "I"   #'forge-browse-issues
        :desc "Browse pull requests"     "P"   #'forge-browse-pullreqs)
       (:prefix ("l" . "list")
        :desc "List repositories"        "r"   #'magit-list-repositories
        :desc "List submodules"          "s"   #'magit-list-submodules
        :desc "List issues"              "i"   #'forge-list-issues
        :desc "List pull requests"       "p"   #'forge-list-pullreqs
        :desc "List notifications"       "n"   #'forge-list-notifications)
       (:prefix ("c" . "create")
        :desc "Initialize repo"          "r"   #'magit-init
        :desc "Clone repo"               "R"   #'magit-clone
        :desc "Commit"                   "c"   #'magit-commit-create
        :desc "Fixup"                    "f"   #'magit-commit-fixup
        :desc "Branch"                   "b"   #'magit-branch-and-checkout
        :desc "Issue"                    "i"   #'forge-create-issue
        :desc "Pull request"             "p"   #'forge-create-pullreq))

      ;; ;;; <leader> i --- insert
      ;; (:prefix-map ("i" . "insert")
      ;;  :desc "Emoji"                         "e"   #'emojify-insert-emoji
      ;;  :desc "Current file name"             "f"   #'+default/insert-file-path
      ;;  :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
      ;;  :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
      ;;  :desc "From evil register"            "r"   #'evil-show-registers
      ;;  :desc "Snippet"                       "s"   #'yas-insert-snippet
      ;;  :desc "Unicode"                       "u"   #'insert-char
      ;;  :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> j --- jump
      (:prefix-map ("j" . "jump")
       :desc "avy goto char timer"        "c"   #'evil-avy-goto-char-timer
       :desc "avy goto 2 char"            "j"   #'evil-avy-goto-char-2
       :desc "avy goto char"              "C"   #'evil-avy-goto-char
       :desc "avy goto line"              "l"   #'evil-avy-goto-line
       :desc "avy goto word"              "w"   #'evil-avy-goto-word-1
       :desc "avy goto symbol"            "o"   #'evil-avy-goto-symbol-1)
      ;;
      ;; ;;; <leader> k --- workspace
      ;; (:when (modulep! :editor workspaces)
      ;;  (:prefix-map ("k" . "workspace")
      ;;   :desc "Display tab bar"           "TAB" #'+workspace/display
      ;;   :desc "Switch workspace"          "."   #'+workspace/switch-to
      ;;   :desc "Switch to last workspace"  "`"   #'+workspace/other
      ;;   :desc "New workspace"             "n"   #'+workspace/new
      ;;   :desc "New name workspace"        "N"   #'+workspace/new-named
      ;;   :desc "Load workspace from file"  "l"   #'+workspace/load
      ;;   :desc "Save workspace to file"    "s"   #'+workspace/save
      ;;   :desc "Delete session"            "x"   #'+workspace/kill-session
      ;;   :desc "Delete this workspace"     "d"   #'+workspace/delete
      ;;   :desc "Rename workspace"          "r"   #'+workspace/rename
      ;;   :desc "Restore last session"      "R"   #'+workspace/restore-last-session
      ;;   :desc "Next workspace"            "]"   #'+workspace/switch-right
      ;;   :desc "Previous workspace"        "["   #'+workspace/switch-left
      ;;   :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
      ;;   :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
      ;;   :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
      ;;   :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
      ;;   :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
      ;;   :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
      ;;   :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
      ;;   :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
      ;;   :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
      ;;   :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))
      ;;
      ;;
      ;;; <leader> m --- mark
      (:prefix-map ("m" . "mark")
       :desc "Set bookmark"               "b"   #'bookmark-set
       :desc "Delete bookmark"            "B"   #'bookmark-delete
       :desc "Mark symbol highlight"      "m"   #'symbol-overlay-put
       :desc "Clear all highlight"        "c"   #'symbol-overlay-remove-all)
      ;;
      ;; ;;; <leader> n --- notes
      ;; (:prefix-map ("n" . "notes")
      ;;  :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
      ;;  :desc "Org agenda"                   "a" #'org-agenda
      ;;  :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
      ;;  :desc "Cancel current org-clock"     "C" #'org-clock-cancel
      ;;  :desc "Open deft"                    "d" #'deft
      ;;  (:when (modulep! :lang org +noter)
      ;;   :desc "Org noter"                  "e" #'org-noter)
      ;;
      ;;  :desc "Find file in notes"           "f" #'+default/find-in-notes
      ;;  :desc "Browse notes"                 "F" #'+default/browse-notes
      ;;  :desc "Org store link"               "l" #'org-store-link
      ;;  :desc "Tags search"                  "m" #'org-tags-view
      ;;  :desc "Org capture"                  "n" #'org-capture
      ;;  :desc "Goto capture"                 "N" #'org-capture-goto-target
      ;;  :desc "Active org-clock"             "o" #'org-clock-goto
      ;;  :desc "Todo list"                    "t" #'org-todo-list
      ;;  :desc "Search notes"                 "s" #'+default/org-notes-search
      ;;  :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
      ;;  :desc "View search"                  "v" #'org-search-view
      ;;  :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
      ;;  :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
      ;;
      ;;  (:when (modulep! :lang org +roam)
      ;;   (:prefix ("r" . "roam")
      ;;    :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
      ;;    :desc "Org Roam Capture"              "c" #'org-roam-capture
      ;;    :desc "Find file"                     "f" #'org-roam-find-file
      ;;    :desc "Show graph"                    "g" #'org-roam-graph
      ;;    :desc "Insert"                        "i" #'org-roam-insert
      ;;    :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
      ;;    :desc "Org Roam"                      "r" #'org-roam
      ;;    (:prefix ("d" . "by date")
      ;;     :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
      ;;     :desc "Today"          "t" #'org-roam-dailies-find-today
      ;;     :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
      ;;     :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))
      ;;
      ;;  (:when (modulep! :lang org +roam2)
      ;;   (:prefix ("r" . "roam")
      ;;    :desc "Open random node"           "a" #'org-roam-node-random
      ;;    :desc "Find node"                  "f" #'org-roam-node-find
      ;;    :desc "Find ref"                   "F" #'org-roam-ref-find
      ;;    :desc "Show graph"                 "g" #'org-roam-graph
      ;;    :desc "Insert node"                "i" #'org-roam-node-insert
      ;;    :desc "Capture to node"            "n" #'org-roam-capture
      ;;    :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
      ;;    :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
      ;;    :desc "Sync database"              "s" #'org-roam-db-sync
      ;;    (:prefix ("d" . "by date")
      ;;     :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
      ;;     :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
      ;;     :desc "Capture date"              "D" #'org-roam-dailies-capture-date
      ;;     :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
      ;;     :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
      ;;     :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
      ;;     :desc "Capture today"             "n" #'org-roam-dailies-capture-today
      ;;     :desc "Goto today"                "t" #'org-roam-dailies-goto-today
      ;;     :desc "Capture today"             "T" #'org-roam-dailies-capture-today
      ;;     :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
      ;;     :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
      ;;     :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))
      ;;
      ;;  (:when (modulep! :lang org +journal)
      ;;   (:prefix ("j" . "journal")
      ;;    :desc "New Entry"           "j" #'org-journal-new-entry
      ;;    :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
      ;;    :desc "Search Forever"      "s" #'org-journal-search-forever)))
      ;;
      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
       :desc "Imenu sidebar"         "i"  #'symbols-outline-smart-toggle
       :desc "Org agenda"            "A"  #'org-agenda
       (:prefix ("a" . "org agenda")
        :desc "Agenda"               "a"  #'org-agenda
        :desc "Todo list"            "t"  #'org-todo-list
        :desc "Tags search"          "m"  #'org-tags-view
        :desc "View search"          "v"  #'org-search-view)
       :desc "File tree"             "f"  #'dirvish-side
       ;; :desc "REPL"               "r"  #'+eval/open-repl-other-window
       ;; :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
       :desc "Dired"                 "-"  #'dired-jump
       ;; (:when (modulep! :editor neotree)
       ;;  :desc "Project sidebar"              "p" #'+neotree/open
       ;;  :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
       ;; (:when (modulep! :editor treemacs)
       ;;  :desc "Open project sidebar" "P" #'treemacs-select-window
       ;;  :desc "Toggle project sidebar" "p" #'+treemacs/toggle)
       :desc "Toggle vterm popup"    "t" #'+vterm/toggle
       :desc "Open vterm here"       "T" #'+vterm/here
       :desc "Toggle eshell popup"   "e" #'+eshell/toggle
       :desc "Open eshell here"      "E" #'+eshell/here)

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
       ;; :desc "Search project"               "n" #'+default/evil-search-to-project
       :desc "Run shell in project"         "'" #'+vterm/here
       ;; :desc "Browse project"               "." #'+default/browse-project
       ;; :desc "Browse other project"         ">" #'doom/browse-in-other-project
       :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
       :desc "Add new project"              "a" #'projectile-add-known-project
       :desc "Compile in project"           "c" #'projectile-compile-project
       :desc "Repeat last command"          "C" #'projectile-repeat-last-command
       :desc "Remove known project"         "d" #'projectile-remove-known-project
       ;; :desc "Discover projects in folder"  "D" #'+default/discover-projects
       :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
       :desc "Find file in project"         "f" #'projectile-find-file
       :desc "Configure project"            "g" #'projectile-configure-project
       :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
       :desc "Kill project buffers"         "k" #'projectile-kill-buffers
       :desc "Find other file"              "o" #'projectile-find-other-file
       :desc "Switch project"               "p" #'projectile-switch-project
       :desc "Find recent project files"    "r" #'projectile-recentf
       :desc "Run project"                  "R" #'projectile-run-project
       :desc "Save project files"           "s" #'projectile-save-project-buffers
       :desc "List project todos"           "t" #'magit-todos-list
       :desc "Test project"                 "T" #'projectile-test-project)

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
       ;; :desc "Restart emacs server"         "d" #'+default/restart-server
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'my/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       ;; :desc "Quick save current session"   "s" #'doom/quicksave-session
       ;; :desc "Restore last session"         "l" #'doom/quickload-session
       ;; :desc "Save session to file"         "S" #'doom/save-session
       ;; :desc "Restore session from file"    "L" #'doom/load-session
       :desc "Restart & restore Emacs"      "r" #'my/restart-and-restore
       :desc "Restart Emacs"                "R" #'my/restart)

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                "b" #'+default/search-buffer
       :desc "Search all open buffers"      "B" #'consult-line-multi
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Search .emacs.d"              "e" #'+default/search-emacsd
       :desc "Locate file"                  "f" #'locate
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Jump to symbol in open buffers" "I" #'consult-imenu-multi
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Jump list"                    "j" #'evil-show-jumps
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       ;; :desc "Look up online"               "o" #'+lookup/online
       ;; :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
       ;; :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
       ;; :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Jump to mark"                 "r" #'evil-show-marks
       :desc "Search buffer"                "s" #'+default/search-buffer
       :desc "Search buffer for thing at point" "S" #'+vertico/search-symbol-at-point
       ;; :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
       ;; :desc "Thesaurus"                    "T" #'+lookup/synonyms
       (:when (fboundp 'vundo)
         :desc "Undo history"               "u" #'vundo)
       :desc "Search Yasnippet"             "y" #'consult-yasnippet)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Fill Column Indicator"        "c" #'global-display-fill-column-indicator-mode
       :desc "Flycheck"                     "f" #'flycheck-mode
       :desc "Indent style"                 "i" #'my/toggle-indent-style
       :desc "Tab bar"                      "t" #'xsort-tab-mode
       :desc "Rainbow delimiter"            "r" #'rainbow-delimiters-mode
       :desc "Soft line wrapping"           "w" #'visual-line-mode)

      ;;; <leader> w --- window
      (:prefix-map ("w" . "window")
       :desc "Alternate window"           "TAB" #'+default/alternate-window
       :desc "Other window"               "w"   #'other-window
       :desc "Tab hydra"                  "t"   #'tabs-fast-switch/body
       :desc "Split window right"         "v"   #'split-window-right
       :desc "Split window right"         "|"   #'split-window-right
       :desc "Split window below"         "s"   #'split-window-below
       :desc "Split window below"         "-"   #'split-window-below
       :desc "Balance window"             "="   #'balance-windows
       :desc "Switch to left"             "h"   #'evil-window-left
       :desc "Switch to right"            "l"   #'evil-window-right
       :desc "Switch to up"               "k"   #'evil-window-up
       :desc "Switch to down"             "j"   #'evil-window-down
       :desc "Kill other window"          "O"   #'ace-delete-other-windows
       :desc "Kill other window"          "o"   #'delete-other-windows
       :desc "Kill window"                "D"   #'ace-delete-window
       :desc "Kill current window"        "d"   #'delete-window))

(provide 'init-keybindings.el)
;;; init-keybindings.el ends here
