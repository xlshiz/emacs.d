;;; init-vc.el --- version control setup. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio transient)
  :init
  ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode...")
        magit-diff-refine-hunk t)
  :config
  ;; Close transient with esc and q
  (define-key transient-map [escape] #'transient-quit-one)
  (define-key transient-map "q" #'transient-quit-one)
  (setq transient-levels-file  (concat my-cache-dir "transient/levels")
        transient-values-file  (concat my-cache-dir "transient/values")
        transient-history-file (concat my-cache-dir "transient/history"))
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 100
        magit-auto-revert-mode t
        git-commit-summary-max-length 100
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)
        magit-display-buffer-function #'+vc-magit-display-buffer-fn ; display buffer fullframe
        magit-bury-buffer-function #'+vc-magit-bury-buffer-fn)      ; bury or kill the current magit buffer
  (define-key magit-mode-map "q" #'+vc/magit-quit)
  (define-key magit-mode-map "Q" #'+vc/magit-quit-all)
  (map! (:map magit-mode-map
         :nv "q" #'+vc/magit-quit
         :nv "Q" #'+vc/magit-quit-all)))

(use-package forge
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat my-etc-dir "forge/forge-database.sqlite"))
  (setq forge-add-default-bindings nil)
  :config
  (require 'emacsql-sqlite)
  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (when (not forge-add-default-bindings)
    (map! :map magit-mode-map [remap magit-browse-thing] #'forge-browse-dwim
          :map magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote
          :map magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch))

  (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
    "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2)
      (if (not (file-executable-p emacsql-sqlite-executable))
          (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                           "See *Compile-Log* buffer for details"))
        ;; HACK Due to changes upstream, forge doesn't initialize completely if
        ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
        ;;      manually after installing it.
        (setq forge--sqlite-available-p t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
        (after! forge-topic
          (dolist (hook forge-bug-reference-hooks)
            (add-hook hook #'forge-bug-reference-setup)))))))

(use-package code-review
  :defer t
  :after magit
  :init
  ;; TODO This needs to either a) be cleaned up or better b) better map things
  ;; to fit
  (after! evil-collection-magit
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    (evil-set-initial-state 'code-review-mode evil-default-state))
  (setq code-review-db-database-file (concat my-etc-dir "code-review/code-review-db-file.sqlite")
        code-review-log-file (concat my-etc-dir "code-review/code-review-error.log")
        code-review-download-dir (concat my-etc-dir "code-review/"))
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review))))

;; Show TODOs in magit
(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package git-commit
  :init
  (add-hook! 'git-commit-mode-hook
            #'(lambda ()
              (yas-activate-extra-mode 'git-commit-mode)))
  :config
  (add-hook! 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state)))))

(use-package smerge-mode
  :defer t
  :diminish smerge-mode
  :init
  (defhydra hydra-smerge-mode (:hint nil
                                     :pre (if (not (bound-and-true-p smerge-mode)) (smerge-mode 1))
                                     ;; Disable `smerge-mode' when quitting hydra if
                                     ;; no merge conflicts remain.
                                     :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue))
  :hook (find-file . (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^<<<<<<< " nil t)
                           (hydra-smerge-mode/body))))))


;; Git modes
(use-package git-modes)

;; Highlight uncommitted changes
(use-package diff-hl
  :commands (diff-hl-next-hunk diff-hl-previous-hunk)
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
  (defhydra hydra-diff-hl (:color pink :hint nil)
    "
_k_: previous _j_: next _m_: mark _g_: goto nth _r_: revert _s_ stage _q_: quit"
    ("j" diff-hl-next-hunk)
    ("k" diff-hl-previous-hunk)
    ("m" diff-hl-mark-hunk)
    ("g" diff-hl-diff-goto-hunk)
    ("r" diff-hl-revert-hunk)
    ("s" diff-hl-stage-current-hunk)
    ("d" diff-hl-show-hunk)
    ("q" nil exit: t))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (setq diff-hl-flydiff-delay 0.5)
  ;; Set fringe style
  (setq-default fringes-outside-margins nil)
  (setq vc-git-diff-switches '("--histogram"))

  ;; Reset faces after changing the color theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (custom-set-faces
               '(diff-hl-insert ((t (:inherit diff-added :background nil))))
               '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
               `(diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil)))))))

  (defun my/diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my/diff-hl-bmp
      (vector (if is-mac-p #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my/diff-hl-fringe-bmp-function)

  (after! magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Set diff-hl-margin-mode
  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist '((insert . " ")
                                         (delete . " ")
                                         (change . " ")
                                         (unknown . " ")
                                         (ignored . " ")))
    ;; Display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :hook ((before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer")))))
  :init
  (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                               (call-interactively 'git-timemachine))
                                             :post (git-timemachine-quit)
                                             :color pink ;; toggle :foreign-keys run
                                             :hint nil)
    "
[_p_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit
"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil exit: t)))

;; Pop up last commit information of current line
(use-package git-messenger
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  (defhydra git-messenger-hydra (:color blue)
    ("s" git-messenger:popup-show "show")
    ("c" git-messenger:copy-commit-id "copy hash")
    ("m" git-messenger:copy-message "copy message")
    ("q" git-messenger:popup-close "quit"))
  :config
  (defun my/git-messenger:format-detail (vcs commit-id author message)
    (if (eq vcs 'git)
        (let ((date (git-messenger:commit-date commit-id))
              (colon (propertize ":" 'face 'font-lock-comment-face)))
          (concat
           (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                   (propertize "Commit" 'face 'font-lock-keyword-face) colon
                   (propertize (substring commit-id 0 8) 'face 'font-lock-string-face)
                   (propertize "Author" 'face 'font-lock-keyword-face) colon
                   (propertize author 'face 'font-lock-string-face)
                   (propertize "Date" 'face 'font-lock-keyword-face) colon
                   (propertize date 'face 'font-lock-string-face))
           (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
           message))
      (git-messenger:format-detail vcs commit-id author message)))
  (defun my/git-messenger:popup-message ()
    "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
    (interactive)
    (let* ((hydra-hint-display-type 'message)
           (vcs (git-messenger:find-vcs))
           (file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos))
           (commit-info (git-messenger:commit-info-at-line vcs file line))
           (commit-id (car commit-info))
           (author (cdr commit-info))
           (msg (git-messenger:commit-message vcs commit-id))
           (popuped-message (if (git-messenger:show-detail-p commit-id)
                                (my/git-messenger:format-detail vcs commit-id author msg)
                              (cl-case vcs
                                (git msg)
                                (svn (if (string= commit-id "-")
                                         msg
                                       (git-messenger:svn-message msg)))
                                (hg msg)))))
      (setq git-messenger:vcs vcs
            git-messenger:last-message msg
            git-messenger:last-commit-id commit-id)
      (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
      (git-messenger-hydra/body)
      (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
             (let ((buffer-name "*git-messenger*"))
               (posframe-show buffer-name
                              :string (concat (propertize "\n" 'face '(:height 0.3))
                                              popuped-message
                                              "\n"
                                              (propertize "\n" 'face '(:height 0.3)))
                              :left-fringe 8
                              :right-fringe 8
                              :internal-border-width 1
                              :internal-border-color (face-foreground 'default)
                              :background-color (face-background 'tooltip nil t))
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (posframe-hide buffer-name))))
            ((fboundp 'popup-tip)
             (popup-tip popuped-message))
            ((fboundp 'lv-message)
             (lv-message popuped-message)
             (unwind-protect
                 (push (read-event) unread-command-events)
               (lv-delete-window)))
            (t (message "%s" popuped-message)))
      (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
  (advice-add #'git-messenger:popup-close :override #'ignore)
  (advice-add #'git-messenger:popup-message :override #'my/git-messenger:popup-message))

(provide 'init-vc)
;;; init-vc ends here
