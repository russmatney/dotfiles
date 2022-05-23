;;; ../dotfiles/emacs/.doom.d/+tasks.el -*- lexical-binding: t; -*-


(map!
 (:leader :desc "Fabb Status" :nv "f" #'fabb-status))

(use-package! fabb
  :config
  (map!
   (:map fabb-mode-map
    :n "i" #'fabb-invoke-ivy
    :n "?" #'fabb-dispatch
    :n "q" #'quit-window)

   (:map fabb-status-mode-map
    "RET" #'fabb-status-task-select
    )

   (:map fabb-task-minor-mode-map
    :n "i" #'fabb-invoke-ivy
    :n "?" #'fabb-dispatch
    :n "r" #'recompile)))
