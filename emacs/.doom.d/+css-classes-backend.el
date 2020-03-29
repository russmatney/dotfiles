;;; ~/dotfiles/emacs/.doom.d/+css-classes-backend.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 's)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example company backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://sixty-north.com/blog/a-more-full-featured-company-mode-backend.html

(defconst simple-completions
  '(#("foobar" 0 1 (:initials "fbrrrr" :summary "herrr"))
    #("foobaz" 0 1 (:initials "fbzzzz" :summary "hezzz"))
    #("foobarbaz" 0 1 (:initials "fbrrzz" :summary "herrrzzz"))))

(defun simple-annotation (s)
  (format " [%s]" (get-text-property 0 :initials s)))

(defun sample-fuzzy-match (prefix candidate)
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun company-simple-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-simple-backend))
    (prefix (company-grab-symbol))
    (candidates (cl-remove-if-not
                 (lambda (c) (sample-fuzzy-match arg c))
                 ;; (lambda (c) (string-prefix-p arg c))
                 simple-completions))
    (meta (format "This value is named %s" arg))
    (annotation (simple-annotation arg))
    (no-cache 't)))

(comment
 (set-company-backend! 'clojurescript-mode
   '(company-capf company-yasnippet company-flow company-simple-backend))
 (set-company-backend! 'emacs-lisp-mode
   '(company-capf company-yasnippet company-flow company-simple-backend))

 (get-text-property 0 :foo #("mytext" 0 1 (:foo "bar")))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS classes backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
