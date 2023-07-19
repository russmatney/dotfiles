;;; ../dotfiles/emacs/.doom.d/+projectile.el -*- lexical-binding: t; -*-


(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/russmatney/" "~/.config/")
        projectile-create-missing-test-files t))
