;;; ../dotfiles/emacs/.doom.d/+vertico.el -*- lexical-binding: t; -*-

(use-package! vertico
  :config
  (map! :map vertico-map
        "C-j" #'vertico-next
        "C-k" #'vertico-previous
        "C-h" #'vertico-directory-delete-word
        "C-l" #'vertico-directory-enter

        "C-n" #'vertico-next-group
        "C-p" #'vertico-previous-group))

(use-package! embark
  :config
  (map! (:map minibuffer-local-map
         [escape] #'minibuffer-keyboard-quit)))
