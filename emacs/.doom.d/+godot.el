;;; ../dotfiles/emacs/.doom.d/+godot.el -*- lexical-binding: t; -*-


;;; lang/gdscript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))


;;
;;; Packages

(use-package! gdscript-mode
  :defer t
  :config

  (setq gdscript-use-tab-indents t)
  (setq gdscript-indent-offset 4)
  (if IS-MAC (setq gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot")
    (setq gdscript-godot-executable "/usr/bin/godot"))
  ;; (setq gdscript-godot-executable "/usr/bin/godot-mono")
  (setq gdscript-gdformat-save-and-format t)

  (set-lookup-handlers! 'gdscript-mode
    :documentation #'gdscript-docs-browse-symbol-at-point)

  ;; when (featurep! +lsp)
  (add-hook 'gdscript-mode-local-vars-hook #'lsp! 'append)

  (map! :localleader
        :map gdscript-mode-map

        (:desc "hydra" "m" #'gdscript-hydra--menu/body)
        (:desc "hydra" "c" #'gdscript-hydra--menu/body)

        (:prefix ("r" . "run")
         :desc "Open project in Godot" "e" #'gdscript-godot-open-project-in-editor
         :desc "Run project" "p" #'gdscript-godot-run-project
         :desc "Run debug" "d" #'gdscript-godot-run-project-debug
         :desc "Run current scene" "s" #'gdscript-godot-run-current-scene)

        (:prefix ("d" . "debug")
         :desc "Add breakpoint" "a"  #'gdscript-debug-add-breakpoint
         :desc "Display breakpoint buffer" "b" #'gdscript-debug-display-breakpoint-buffer
         :desc "Remove breakpoint" "d" #'gdscript-debug-remove-breakpoint
         :desc "Continue execution" "c" #'gdscript-debug-continue
         :desc "Next" "n" #'gdscript-debug-next
         :desc "Step" "s" #'gdscript-debug-step)

        (:prefix ("h" . "help")
         :desc "Browse online API" "b" #'gdscript-docs-browse-api
         :desc "Browse API at point" "f" #'gdscript-docs-browse-symbol-at-point)

        (:prefix ("f" . "format")
         :desc "Format buffer" "b" #'gdscript-format-buffer
         :desc "Format region" "r" #'gdscript-format-region)))
