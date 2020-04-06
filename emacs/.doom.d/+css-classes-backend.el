;;; ~/dotfiles/emacs/.doom.d/+css-classes-backend.el -*- lexical-binding: t; -*-

;;; original gist:
;; https://gist.github.com/russmatney/83ba29203ab25066305d080f39e305ad

(require 's)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defconst css-imenu-generic-expression
  "^[ \t]*\\([[:word:].:#, \t-]+\\)\\s-*{"
  "Regular expression matching any selector. Used by imenu.")

;; TY: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun matches-in-file (regexp filename)
  "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (current-buffer)
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (save-restriction
              (widen)
              (goto-char 1)
              (while (search-forward-regexp regexp nil t 1)
                ;; TODO parse styles from css here as metadata
                (push (match-string 0) matches))))))
      matches)))

(defun css-file-path ()
  ;; TODO scan project for css? read from .dir-locals.el?
  (file-truename "~/russmatney/yodo/public/css/main-built.css"))

(defun trim-class-name (s)
  "The regex currently includes leading space and dots,
and trailing open-brackets. This trims them."
  (s-chop-suffix " {" (s-chop-prefix "." s)))

(defun get-classes ()
  (let* ((filepath (css-file-path))
         (parsed (matches-in-file css-imenu-generic-expression filepath))
         (filtered (cl-remove-if-not
                    (lambda (s) (s-prefix? "." s))
                    parsed)))
    (cl-map 'list #'trim-class-name filtered)))

(comment
 (get-classes))

(defun css-classes-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'css-classes-backend))
    (prefix (company-grab-symbol))
    (candidates (cl-remove-if-not
                 ;; (lambda (c) (sample-fuzzy-match arg c))
                 (lambda (c) (string-prefix-p arg c))
                 (get-classes)))))

(comment
 (set-company-backend! 'clojurescript-mode
   '(company-capf company-yasnippet company-flow css-classes-backend))
 (set-company-backend! 'emacs-lisp-mode
   '(company-capf company-yasnippet company-flow css-classes-backend)))
