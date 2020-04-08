;;; ~/dotfiles/emacs/.doom.d/+lispy.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode)

  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

(evil-define-command rm/lispyville-insert-at-end-of-list (count)
  "same as `lispyville-insert-at-end-of-list', but adds a newline."
  (interactive "<c>")
  ;; TODO if already at the end of the list, add a newline above
  (when (lispyville--out-forward (or count 1))
    (backward-char)
    (newline-and-indent)
    (evil-change-state lispyville-preferred-state)))

(use-package! lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  (clojure-mode . lispy-mode)
  (lisp-mode . lispy-mode)

  :bind (:map lispy-mode-map
          ("M-L" . lispyville-beginning-of-next-defun)
          ("M-n" . nil)))

;; https://github.com/noctuid/lispyville
(use-package! lispyville
  :hook
  (lispy-mode . lispyville-mode)

  :bind (:map lispyville-mode-map
          ("M-L" . lispyville-beginning-of-next-defun)
          ("M-n" . nil))

  :config
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
    "M-o" 'rm/lispyville-insert-at-end-of-list)
  (evil-define-key 'visual lispyville-mode-map
    "(" 'lispy-parens)

  (setq
   lispy-safe-actions-ignore-strings t
   lispy-safe-actions-ignore-comments t)

  ;; TODO should fix this some other way
  (remove-hook 'clojure-mode-hook 'parinfer-mode)
  (remove-hook 'emacs-lisp-mode-hook 'parinfer-mode))
