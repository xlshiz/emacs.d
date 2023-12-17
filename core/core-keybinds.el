;;; core-keybinds.el --- defaults for keybinds -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
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

(defun my-keyword-name (keyword)
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
                          (my-lookup-key
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
(defmacro my--define-leader-key (&rest keys)
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
    ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
    ;; emacs state)
    `(general-define-key
      :states '(normal visual motion emacs insert)
      :major-modes t
      :prefix my-localleader-key
      :non-normal-prefix my-localleader-alt-key
      ,@args))

;; PERF: We use a prefix commands instead of general's
;;   :prefix/:non-normal-prefix properties because general is incredibly slow
;;   binding keys en mass with them in conjunction with :states -- an effective
;;   doubling of Doom's startup time!
(define-prefix-command 'my/leader 'my-leader-map)
(define-key my-leader-map [override-state] 'all)

;; Bind `my-leader-key' and `my-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
  (defun my-init-leader-keys-h ()
    "Bind `my-leader-key' and `my-leader-alt-key'."
    (let ((map general-override-mode-map))
      (evil-define-key* '(normal visual motion) map (kbd my-leader-key) 'my/leader)
      (evil-define-key* '(emacs insert) map (kbd my-leader-alt-key) 'my/leader)
      (general-override-mode +1))))

;;
;;; `map!' macro

(defvar my-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun my--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`my-evil-state-alist' to customize this."
  (cl-loop for l across (my-keyword-name keyword)
           if (assq l my-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar my--map-forms nil)
(defvar my--map-fn nil)
(defvar my--map-batch-forms nil)
(defvar my--map-state '(:dummy t))
(defvar my--map-parent-state nil)
(defvar my--map-evil-p nil)
(after! evil (setq my--map-evil-p t))

(defun my--map-process (rest)
  (let ((my--map-fn my--map-fn)
        my--map-state
        my--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (my--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (my--map-commit)
                  (setq my--map-fn 'my--define-leader-key))
                 (:localleader
                  (my--map-commit)
                  (setq my--map-fn 'define-localleader-key!))
                 (:after
                  (my--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (my--map-set :keymaps `(quote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (my--map-nested (list (intern (my-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "my-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            my--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (my--map-set (if my--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          my--map-forms)))
                 (_
                  (condition-case _
                      (my--map-def (pop rest) (pop rest)
                                     (my--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((my--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (my--map-commit)
    (macroexp-progn (nreverse (delq nil my--map-forms)))))

(defun my--map-append-keys (prop)
  (let ((a (plist-get my--map-parent-state prop))
        (b (plist-get my--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun my--map-nested (wrapper rest)
  (my--map-commit)
  (let ((my--map-parent-state (my--map-state)))
    (push (if wrapper
              (append wrapper (list (my--map-process rest)))
            (my--map-process rest))
          my--map-forms)))

(defun my--map-set (prop &optional value)
  (unless (equal (plist-get my--map-state prop) value)
    (my--map-commit))
  (setq my--map-state (plist-put my--map-state prop value)))

(defun my--map-def (key def &optional states desc)
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
          (alist-get state my--map-batch-forms)))
  t)

(defun my--map-commit ()
  (when my--map-batch-forms
    (cl-loop with attrs = (my--map-state)
             for (state . defs) in my--map-batch-forms
             if (or my--map-evil-p (not state))
             collect `(,(or my--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) my--map-forms))
    (setq my--map-batch-forms nil)))

(defun my--map-state ()
  (let ((plist
         (append (list :prefix (my--map-append-keys :prefix)
                       :infix  (my--map-append-keys :infix)
                       :keymaps
                       (append (plist-get my--map-parent-state :keymaps)
                               (plist-get my--map-state :keymaps)))
                 my--map-state
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
    (my--map-process rest)))

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


(provide 'core-keybinds)
;;; core-keybinds.el ends here
