;;; core-keybinds.el --- defaults for keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).
;;
;;; Code:

(defvar my-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

;;
;;; Global keybind settings

(cond
 (is-mac-p
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (is-windows-p
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs greated separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
(define-key key-translation-map [?\C-i]
  (cmd! (if (let ((keys (this-single-command-raw-keys)))
              (and keys
                   (not (cl-position 'tab    keys))
                   (not (cl-position 'kp-tab keys))
                   (display-graphic-p)
                   ;; Fall back if no <C-i> keybind can be found, otherwise
                   ;; we've broken all pre-existing C-i keybinds.
                   (let ((key
                          (doom-lookup-key
                           (vconcat (cl-subseq keys 0 -1) [C-i]))))
                     (not (or (numberp key) (null key))))))
            [C-i] [?\C-i])))


;;; General + leader/localleader keys

(require 'general)
;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
(general-evil-setup)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro doom--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (my-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key my-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t my-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t my-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `my-localleader-key' and `my-localleader-alt-key' to change the
localleader prefix."
  (if (t)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix my-localleader-key
        :non-normal-prefix my-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix my-localleader-alt-key
      ,@args)))

;; PERF: We use a prefix commands instead of general's
;;   :prefix/:non-normal-prefix properties because general is incredibly slow
;;   binding keys en mass with them in conjunction with :states -- an effective
;;   doubling of Doom's startup time!
(define-prefix-command 'my/leader 'my-leader-map)
(define-key my-leader-map [override-state] 'all)

;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
  (defun doom-init-leader-keys-h ()
    "Bind `doom-leader-key' and `doom-leader-alt-key'."
    (let ((map general-override-mode-map))
      (evil-define-key* '(normal visual motion) map (kbd my-leader-key) 'my/leader)
      (evil-define-key* '(emacs insert) map (kbd my-leader-alt-key) 'my/leader)
      (general-override-mode +1))))

;;
;;; `map!' macro

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun doom--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (doom-keyword-name keyword)
           if (assq l doom-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar doom--map-forms nil)
(defvar doom--map-fn nil)
(defvar doom--map-batch-forms nil)
(defvar doom--map-state '(:dummy t))
(defvar doom--map-parent-state nil)
(defvar doom--map-evil-p nil)
(after! evil (setq doom--map-evil-p t))

(defun doom--map-process (rest)
  (let ((doom--map-fn doom--map-fn)
        doom--map-state
        doom--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (doom--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (doom--map-commit)
                  (setq doom--map-fn 'doom--define-leader-key))
                 (:localleader
                  (doom--map-commit)
                  (setq doom--map-fn 'define-localleader-key!))
                 (:after
                  (doom--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (doom--map-set :keymaps `(quote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (doom--map-nested (list (intern (doom-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "doom-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            doom--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (doom--map-set (if doom--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          doom--map-forms)))
                 (_
                  (condition-case _
                      (doom--map-def (pop rest) (pop rest)
                                     (doom--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((doom--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (doom--map-commit)
    (macroexp-progn (nreverse (delq nil doom--map-forms)))))

(defun doom--map-append-keys (prop)
  (let ((a (plist-get doom--map-parent-state prop))
        (b (plist-get doom--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun doom--map-nested (wrapper rest)
  (doom--map-commit)
  (let ((doom--map-parent-state (doom--map-state)))
    (push (if wrapper
              (append wrapper (list (doom--map-process rest)))
            (doom--map-process rest))
          doom--map-forms)))

(defun doom--map-set (prop &optional value)
  (unless (equal (plist-get doom--map-state prop) value)
    (doom--map-commit))
  (setq doom--map-state (plist-put doom--map-state prop value)))

(defun doom--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (my-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state doom--map-batch-forms)))
  t)

(defun doom--map-commit ()
  (when doom--map-batch-forms
    (cl-loop with attrs = (doom--map-state)
             for (state . defs) in doom--map-batch-forms
             if (or doom--map-evil-p (not state))
             collect `(,(or doom--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) doom--map-forms))
    (setq doom--map-batch-forms nil)))

(defun doom--map-state ()
  (let ((plist
         (append (list :prefix (doom--map-append-keys :prefix)
                       :infix  (doom--map-append-keys :infix)
                       :keymaps
                       (append (plist-get doom--map-parent-state :keymaps)
                               (plist-get doom--map-state :keymaps)))
                 doom--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

Properties
  :leader [...]                   an alias for (:prefix my-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #'dosomething)"
  (when (or (bound-and-true-p byte-compile-current-file)
            (not noninteractive))
    (doom--map-process rest)))

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  ;; (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements my-leader-key "<leader>")
  (which-key-add-key-based-replacements my-localleader-key "<localleader>"))

;; (use-package general
;;   :config
;;   (general-evil-setup)
;;   (general-create-definer my/global-leader-define
;;     :states '(normal visual motion insert evilified)
;;     :keymaps 'override
;;     :prefix my-global-leader-prefix
;;     :non-normal-prefix my-global-leader-alt-prefix)
;;   (general-create-definer my/local-leader-define
;;     :states '(normal visual motion insert evilified)
;;     :keymaps 'override
;;     :prefix my-local-leader-prefix
;;     :non-normal-prefix my-local-leader-alt-prefix))

;; (use-package which-key
;;   :diminish which-key-mode "Ⓚ"
;;   :hook (after-init . which-key-mode)
;;   :config
;;   (setq which-key-idle-delay 0.3
;;         which-key-compute-remaps t
;;         which-key-min-display-lines 1
;;         which-key-add-column-padding 1
;;         which-key-max-display-columns nil
;;         which-key-sort-uppercase-first nil
;;         which-key-side-window-max-width 0.33
;;         which-key-side-window-max-height 0.25
;;         which-key-sort-order #'which-key-prefix-then-key-order)
;;   (which-key-setup-side-window-bottom)
;;   (dolist (item '((("SPC" . nil) . ("␣" . nil))
;;                   (("TAB" . nil) . ("↹" . nil))
;;                   (("RET" . nil) . ("⏎" . nil))
;;                   (("DEL" . nil) . ("⌫" . nil))
;;                   (("<up>" . nil) . ("↑" . nil))
;;                   (("<down>" . nil) . ("↓" . nil))
;;                   (("<left>" . nil) . ("←" . nil))
;;                   (("<right>" . nil) . ("→" . nil))
;;                   (("deletechar" . nil) . ("⌦" . nil))
;;                   ;; rename winum-select-window-1 entry to 1..9
;;                   (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9"))
;;                   ;; hide winum-select-window-[2-9] entries
;;                   ((nil . "winum-select-window-[2-9]") . t)))
;;     (cl-pushnew item which-key-replacement-alist :test #'equal))
;;   (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))


(provide 'core-keybinds)
;;; doom-keybinds.el ends here
