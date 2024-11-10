;;; ~/dotfiles/emacs/.doom.d/+lispy.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode)
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode)
  (gerbil-mode . aggressive-indent-mode)

  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)
  (setq cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil)
  )

(evil-define-command rm/lispyville-insert-at-end-of-list (count)
  "same as `lispyville-insert-at-end-of-list', but adds a newline."
  (interactive "<c>")
  ;; TODO if already at the end of the list, add a newline above
  (when (lispyville--out-forward (or count 1))
    (backward-char)
    (newline-and-indent)
    (evil-change-state lispyville-preferred-state)))

;; (use-package! lispy
;;   :hook
;;   (emacs-lisp-mode . lispy-mode)
;;   (clojure-mode . lispy-mode)
;;   (lisp-mode . lispy-mode)

;;   :bind (:map lispy-mode-map
;;           ("M-n" . nil)
;;           ))

(map!
 :after lispyville
 :map lispyville-mode-map
 :n "M-L" #'lispyville-beginning-of-next-defun
 :v "(" #'lispy-parens)

(use-package! lispyville
  :hook
  ;; (lispy-mode . lispyville-mode)
  (emacs-lisp-mode . lispyville-mode)
  (clojure-mode . lispyville-mode)
  (lisp-mode . lispyville-mode)
  (gerbil-mode . lispyville-mode)

  :config

  ;; (map! (:map +tree-sitter-inner-text-objects-map
  ;;             "l" nil)
  ;;       (:map +tree-sitter-outer-text-objects-map
  ;;             "l" nil))

  (lispyville-set-key-theme
   '(operators
     c-w
     c-u
     prettify
     text-objects
     (atom-motions t)
     additional-motions
     additional
     additional-insert
     additional-wrap
     commentary
     slurp/barf-cp))

  (evil-define-key 'normal lispyville-mode-map
    (kbd "M-o") 'rm/lispyville-insert-at-end-of-list)

  (setq
   lispy-safe-actions-ignore-strings t
   lispy-safe-actions-ignore-comments t)

  ;; TODO should fix this some other way
  (remove-hook 'clojure-mode-hook 'parinfer-mode)
  (remove-hook 'emacs-lisp-mode-hook 'parinfer-mode))
