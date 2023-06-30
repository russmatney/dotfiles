;;; ../dotfiles/emacs/.doom.d/+kubernetes.el -*- lexical-binding: t; -*-

(use-package! kubernetes)
(use-package! kubernetes-evil
  :after kubernetes)

(use-package! kele
  :config
  (kele-mode 1))
