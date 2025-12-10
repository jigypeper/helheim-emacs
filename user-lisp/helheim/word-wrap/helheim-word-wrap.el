;;; helheim-word-wrap.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Ported from DOOM Emacs.
;;
;; This module defines `+word-wrap-mode' minor-mode, which intelligently
;; wraps long lines in the buffer without modifying the buffer content.
;;
;; Do not `require' this module manually in you `init.el' file —
;; `+word-wrap-mode' is autoloaded, and bound to "SPC t w".
;;
;;; Code:

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

(defcustom +word-wrap-disabled-modes '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable `+word-wrap-mode'.")

(defcustom +word-wrap-visual-modes '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use `adaptive-wrap-prefix-mode'.")

(defcustom +word-wrap-text-modes '( text-mode markdown-mode markdown-view-mode
                                    gfm-mode gfm-view-mode rst-mode
                                    latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

;; (when (memq 'visual-line-mode text-mode-hook)
;;   (remove-hook 'text-mode-hook #'visual-line-mode)
;;   (add-hook 'text-mode-hook #'+word-wrap-mode))

;;; .
(provide 'helheim-word-wrap)
;;; helheim-word-wrap.el ends here
