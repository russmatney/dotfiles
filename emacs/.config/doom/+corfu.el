;;; ../../dotfiles/emacs/.config/doom/+corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  :config

  (setq corfu-preselect 'first)
  (setq +corfu-want-ret-to-confirm t)
  (setq corfu-auto nil))

(map! :when (modulep! :completion corfu)
      :after corfu
      :map corfu-map
      :i "C-SPC" #'+corfu/smart-sep-toggle-escape
      "C-SPC" #'+corfu/smart-sep-toggle-escape
      :i "C-S-s" #'+corfu/move-to-minibuffer)
