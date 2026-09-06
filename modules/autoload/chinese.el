;;; chinese.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +intel-consult-regex-pinyin-builder (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))

;;;###autoload
(defun +chinese-pyim-converter (str)
  (cond ((<= (length str) 1) str)
        ((equal str "【】") str)
        ((equal str "】【") str)
        ((equal (substring str -1) "【")
         (substring str 0 1))
        ((equal (substring str -1) "】")
         (substring str -2 -1))
        (t str))
  )
