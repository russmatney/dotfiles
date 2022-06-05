;;; ../dotfiles/emacs/.doom.d/+ink.el -*- lexical-binding: t; -*-


(add-hook 'ink-mode-hook 'flymake-mode)
(add-hook 'ink-play-mode-hook 'toggle-truncate-lines)

(use-package! ink-mode
  :config
  (toggle-truncate-lines)
  (auto-fill-mode 1)
  ;; TODO this overwrites the localleader everywhere :/
  ;; (map!
  ;;  (:after ink-mode
  ;;   (:mode ink-mode
  ;;    (:leader "m" #'hydra-ink/body))))
  )

(defhydra hydra-ink (:exit t)
  ("c"  ink-play "play" :column "play")
  ("h"  ink-display-manual "Ink Manual" :column "help"))
