;;; ../dotfiles/emacs/.doom.d/+godot.el -*- lexical-binding: t; -*-

(use-package! gdscript-mode
  :config
  (setq gdscript-use-tab-indents nil)
  (setq gdscript-indent-offset 2)
  ;; (setq gdscript-godot-executable "/usr/bin/godot-mono")
  (setq gdscript-godot-executable "/usr/bin/godot")
  (setq gdscript-gdformat-save-and-format t))
