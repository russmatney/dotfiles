;;; puzz-mode.el --- Major mode for .puzz game files -*- lexical-binding: t; -*-

;;; Commentary:

;; A simple major mode for editing .puzz files.
;; Inherits from `text-mode' — plain text editing with word wrap and
;; spell-check support.  Extend this file as the format grows.

;;; Code:

(defgroup puzz nil
  "Major mode for .puzz game files."
  :group 'text)

;;;###autoload
(define-derived-mode puzz-mode text-mode "Puzz"
  "Major mode for editing .puzz game files.
Inherits from `text-mode'."
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.puzz\\'" . puzz-mode))

(provide 'puzz-mode)
;;; puzz-mode.el ends here
