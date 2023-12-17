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
(defun +intel-ivy-regex-pinyin-builder (str)
  (or (+intel-pinyin-to-utf8 str)
      (ivy--regex-plus str)
      (ivy--regex-ignore-order str)
      ))

;;;###autoload
(defun +intel-consult-regex-pinyin-builder (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))

;;;###autoload
(defun +intel-pinyinlib-build-regexp-string (str)
  (progn
    (cond ((equal str ".*")
           ".*")
          (t
           (pinyinlib-build-regexp-string str t))))
  )

;;;###autoload
(defun +intel-pinyin-regexp-helper (str)
  (cond ((equal str " ")
         ".*")
        ((equal str "")
         nil)
        (t
         str)))

;;;###autoload
(defun +intel-pinyin-to-utf8 (str)
  (cond ((equal 0 (length str))
         nil)
        ((equal (substring str 0 1) "!")
         (mapconcat '+intel-pinyinlib-build-regexp-string
                    (remove nil (mapcar '+intel-pinyin-regexp-helper (split-string
                                                                  (substring str 1) "")))
                    ""))
        (t nil)))

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
