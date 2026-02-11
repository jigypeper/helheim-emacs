;;; helheim-word-wrap.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module defines `+word-wrap-mode' minor-mode, which intelligently
;; wraps long lines in the buffer without modifying the buffer content.
;;
;; Ported from DOOM Emacs.
;;
;;; Customization

(defcustom +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

`double'             indent by twice the major-mode indentation
`single'             indent by the major-mode indentation
positive integer   indent by this fixed amount
negative integer   dedent by this fixed amount

Otherwise no extra indentation will be used."
  :type '(choice (const :tag "Double indentation" double)
                 (const :tag "Single indentation" single)
                 integer)
  :group 'helheim)

(defcustom +word-wrap-fill-style 'soft
  "How to handle `fill-column' in `+word-wrap-mode'.

When `auto', long lines will soft-wrap at `fill-column'. If `auto-fill-mode'
is enabled, its behaviour will not be affected.

When `soft', long lines will soft-wrap at `fill-column' and `auto-fill-mode'
will be forcibly disabled.

Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected."
  :type '(choice (const :tag "Auto (respect auto-fill)" auto)
                 (const :tag "Soft (disable auto-fill)" soft)
                 (const :tag "Window margin (default)" nil))
  :group 'helheim)

(defcustom +word-wrap-disabled-modes '(fundamental-mode
                                       so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable `+word-wrap-mode'.")

(defcustom +word-wrap-visual-modes '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use `adaptive-wrap-prefix-mode'.")

(defcustom +word-wrap-text-modes '(text-mode
                                   markdown-mode markdown-view-mode
                                   gfm-mode gfm-view-mode
                                   rst-mode
                                   latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

;; (when (memq 'visual-line-mode text-mode-hook)
;;   (remove-hook 'text-mode-hook #'visual-line-mode)
;;   (add-hook 'text-mode-hook #'+word-wrap-mode))

;;; Code

(defvar-local +word-wrap--major-mode-is-visual nil)
(defvar-local +word-wrap--major-mode-is-text nil)
(defvar-local +word-wrap--enable-adaptive-wrap-mode nil)
(defvar-local +word-wrap--enable-visual-line-mode nil)
(defvar-local +word-wrap--enable-visual-fill-mode nil)
(defvar-local +word-wrap--disable-auto-fill-mode nil)
(defvar-local +word-wrap--major-mode-indent-var nil)

(defvar adaptive-wrap-extra-indent)
(defun +word-wrap--adjust-extra-indent-a (fn beg end)
  "Contextually adjust extra word-wrap indentation."
  (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg)))
    (funcall fn beg end)))

(defun +word-wrap--calc-extra-indent (position)
  "Calculate extra word-wrap indentation at point."
  (if (or +word-wrap--major-mode-is-text
          (or (hel-comment-at-pos-p position)
              (hel-string-at-pos-p position)))
      0
    (pcase +word-wrap-extra-indent
      ('double (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
      ('single (symbol-value +word-wrap--major-mode-indent-var))
      ((and (pred integerp) fixed)
       fixed)
      (_ 0))))

;;;###autoload
(define-minor-mode +word-wrap-mode
  "Wrap long lines in the buffer with language-aware indentation.

This mode configures `adaptive-wrap', `visual-line-mode' and
`visual-fill-column-mode' to wrap long lines without modifying the buffer
content. This is useful when dealing with legacy code which contains
gratuitously long lines, or running emacs on your wrist-phone.

Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+word-wrap-extra-indent'.

Long lines will wrap at the window margin by default, or can optionally be
wrapped at `fill-column' by configuring `+word-wrap-fill-style'."
  :init-value nil
  (if +word-wrap-mode
      (progn
        (setq +word-wrap--major-mode-is-visual (memq major-mode +word-wrap-visual-modes)
              +word-wrap--major-mode-is-text (memq major-mode +word-wrap-text-modes)
              +word-wrap--enable-adaptive-wrap-mode (and (not (bound-and-true-p adaptive-wrap-prefix-mode))
                                                         (not +word-wrap--major-mode-is-visual))
              +word-wrap--enable-visual-line-mode (not (bound-and-true-p visual-line-mode))
              +word-wrap--enable-visual-fill-mode (and (not (bound-and-true-p visual-fill-column-mode))
                                                       (memq +word-wrap-fill-style '(auto soft)))
              +word-wrap--disable-auto-fill-mode (and (bound-and-true-p auto-fill-function)
                                                      (eq +word-wrap-fill-style 'soft)))
        (unless +word-wrap--major-mode-is-visual
          (when (require 'dtrt-indent nil t)
            ;; for dtrt-indent--search-hook-mapping
            ;; TODO: Generalize this?
            (setq +word-wrap--major-mode-indent-var
                  (let ((indent-var (caddr (dtrt-indent--search-hook-mapping major-mode))))
                    (if (listp indent-var)
                        (car indent-var)
                      indent-var)))
            (advice-add #'adaptive-wrap-fill-context-prefix :around #'+word-wrap--adjust-extra-indent-a)))
        ;;
        (when +word-wrap--enable-adaptive-wrap-mode (adaptive-wrap-prefix-mode +1))
        (when +word-wrap--enable-visual-line-mode (visual-line-mode +1))
        (when +word-wrap--enable-visual-fill-mode (visual-fill-column-mode +1))
        (when +word-wrap--disable-auto-fill-mode (auto-fill-mode -1)))
    ;; else
    (unless +word-wrap--major-mode-is-visual
      (advice-remove #'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a))
    (when +word-wrap--enable-adaptive-wrap-mode (adaptive-wrap-prefix-mode -1))
    (when +word-wrap--enable-visual-line-mode (visual-line-mode -1))
    (when +word-wrap--enable-visual-fill-mode (visual-fill-column-mode -1))
    (when +word-wrap--disable-auto-fill-mode (auto-fill-mode +1))))

(defun +word-wrap--enable-global-mode ()
  "Enable `+word-wrap-mode' for `+word-wrap-global-mode'.
Wrapping will be automatically enabled in all modes except special modes,
or modes explicitly listed in `+word-wrap-disabled-modes'."
  (unless (or (eq (get major-mode 'mode-class) 'special)
              (memq major-mode +word-wrap-disabled-modes))
    (+word-wrap-mode +1)))

;;;###autoload
(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode
  +word-wrap--enable-global-mode)

;;; .
(provide 'helheim-word-wrap)
;;; helheim-word-wrap.el ends here
