;;; -*- lexical-binding: t; -*-
(require 'helheim-word-wrap)

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
        (setq +word-wrap--major-mode-is-visual (memq major-mode +word-wrap-visual-modes))
        (setq +word-wrap--major-mode-is-text (memq major-mode +word-wrap-text-modes))
        (setq +word-wrap--enable-adaptive-wrap-mode (and (not (bound-and-true-p adaptive-wrap-prefix-mode))
                                                         (not +word-wrap--major-mode-is-visual)))
        (setq +word-wrap--enable-visual-line-mode (not (bound-and-true-p visual-line-mode)))
        (setq +word-wrap--enable-visual-fill-mode (and (not (bound-and-true-p visual-fill-column-mode))
                                                       (memq +word-wrap-fill-style '(auto soft))))
        (setq +word-wrap--disable-auto-fill-mode (and (bound-and-true-p auto-fill-function)
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

        (when +word-wrap--enable-adaptive-wrap-mode (adaptive-wrap-prefix-mode +1))
        (when +word-wrap--enable-visual-line-mode (visual-line-mode +1))
        (when +word-wrap--enable-visual-fill-mode (visual-fill-column-mode +1))
        (when +word-wrap--disable-auto-fill-mode (auto-fill-mode -1)))

    ;; else disable +word-wrap-mode
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
