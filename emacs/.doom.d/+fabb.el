;;; ../dotfiles/emacs/.doom.d/+tasks.el -*- lexical-binding: t; -*-


(map!
 (:leader :desc "Fabb Status" :nv "f" #'fabb-status))

(map!
 (:after fabb
  (:map fabb-mode-map
   :n "i" #'fabb-invoke-ivy
   :n "?" #'fabb-dispatch
   :n "f" #'fabb-dispatch
   :n "q" #'quit-window)

  (:map fabb-status-mode-map
   :n "r" #'fabb-status-invoke-task-and-show-buffer
   :n "R" #'fabb-status-invoke-task-in-background
   :n "e" #'fabb-status-edit-and-invoke-task
   :n "RET" #'fabb-status-show-task-buffer
   :n "x" #'fabb-kill-fabb-buffers)

  (:map fabb-task-mode-map
   :n "i" #'fabb-invoke-ivy
   :n "?" #'fabb-dispatch

   :n "e" #'fabb-task-edit-and-reinvoke-task
   :n "r" #'fabb-task-reinvoke-task-prompt
   :n "R" #'fabb-task-reinvoke-task-no-prompt))

 (:map compilation-mode-map
  :n "C-k" nil
  :n "C-j" nil)
 (:map compilation-minor-mode-map
  :n "C-k" nil
  :n "C-j" nil))

(use-package! fabb)
