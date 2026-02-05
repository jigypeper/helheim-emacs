;;; -*- lexical-binding: t -*-

;;;###autoload
(defun helheim-keyboard-quit ()
  "Improved `keyboard-quit'."
  (interactive)
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (abort-recursive-edit))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          (t (keyboard-quit)))))
