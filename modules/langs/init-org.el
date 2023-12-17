;;; init-org.el --- Initialize org configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar my/org-dir "~/workdir/docs/org/"
  "The directory where org files are kept.")

(defvar my/org-notes-file (concat my/org-dir "notes.org")
  "The org notes file.")

(defvar my/org-task-file (concat my/org-dir "tasks.org")
  "The org task file.")

(defvar my/org-idea-file (concat my/org-dir "ideas.org")
  "The org idea file.")

(defvar my/org-reading-file (concat my/org-dir "books.org")
  "The org idea file.")

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c l" . org-store-link)
         ([tab] . org-cycle))
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :hook (org-mode . lsp!)
  :preface
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "#ee6363" :weight bold))
                                 ("DOING" . (:foreground "#3a81c3" :weight bold))
                                 ("HANGUP" . (:foreground "red" :weight bold))
                                 ("DONE" . (:foreground "#7ccd7c" :weight bold))
                                 ("CANCEL"  . (:foreground "yellow" :weight bold)))
        org-ellipsis " ▼ "
        org-log-done 'time
        org-src-fontify-natively t
        org-clock-string "计时:"
        org-closed-string "已关闭:"
        org-deadline-string "最后期限:"
        org-scheduled-string "计划任务:"
        ;; Fast TODO Selection
        org-use-fast-todo-selection t
        ;; record timestamp when a task moves to the DONE state
        org-log-done 'time
        ;; Log time when rescheduling an entry.
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-into-drawer t
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Don't clock out when moving task to a done state
        org-clock-out-when-done nil
        ;; Save the running clock and all clock history when exiting Emacs,load it on startup
        org-clock-persist t
        org-confirm-babel-evaluate nil
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-startup-indented t
        org-pretty-entities t)

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; Pomodoro
  (use-package org-pomodoro
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

  (use-package org-capture
    :config
    (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
          org-capture-templates
          '(("t" "tasks" entry (file+headline my/org-task-file "Work")
             "* %^{Scope of task||TODO [#A]|STUDY [#A]|MEET with} %^{Title} %^g\n DEADLINE: %^t\n :PROPERTIES:\n :CONTEXT: %a\n:CAPTURED: %U\n :END:\n\n %i %?"
             :empty-lines 1)
            ("n" "notes" entry (file+headline my/org-notes-file "Notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("i" "ideas" entry (file+headline my/org-idea-file "Ideas")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("r" "reading" entry (file+olp my/org-reading-file "阅读书目" "2020")
             "* TODO %^{The book's name} %^g\n%^{STYLE}p"
             :empty-lines 1))))

  (use-package org-agenda
    :config
    (setq org-agenda-files my/org-dir
          ;; Set the agenda view to show the tasks on day/week/month/year
          org-agenda-span 'week
          ;; only keep agenda window,delete all other window
          org-agenda-window-setup 'only-window
          org-agenda-todo-ignore-scheduled t
          org-agenda-todo-ignore-deadlines t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-todo-list-sublevels t
          ;; format 9:30-->09:30
          org-agenda-time-leading-zero nil
          org-agenda-format-date "%Y-%m-%d %a----------------------------------------------------------------"
          ;; Custom commands for the agenda -- start with a clean slate.
          org-agenda-custom-commands nil
          ;; Do not dim blocked tasks
          org-agenda-dim-blocked-tasks nil
          ;; Compact the block agenda view
          org-agenda-compact-blocks t
          org-agenda-scheduled-leaders '("计划任务 " "应在 %02d 天前开始 ")
          org-agenda-deadline-leaders '("过期任务 " "将在 %02d 天后到期 " "已过期 %02d 天 "))

    (use-package org-archive
      :config
      ;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
      ;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
      ;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
      ;; (setq org-archive-default-command 'org-archive-subtree)
      (setq org-archive-default-command 'org-archive-set-tag)))

  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (ruby . t)))
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))
  ;; 让中文也可以不加空格就使用行内格式
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
  (setq org-use-sub-superscripts "{}"))

(provide 'init-org)

;;; init-org.el ends here
