;;; ../dotfiles/emacs/.doom.d/+hydra.el -*- lexical-binding: t; -*-

;; HAIL HYDRA
;; Name from: https://github.com/abo-abo/hydra
;; Code derived from: https://github.com/Camsbury/config/blob/master/emacs-conf/core/commands.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running Shell Commands in Buffers

(defun run-shell-command-in-background (buff-name command)
  "runs a shell command async in a background buffer"
  (interactive "sBuffer name: \nsCommand: ")
  (async-shell-command
   command
   (generate-new-buffer-name (concat "*" buff-name "*"))))

;; TODO option for streaming content from, e.g., a watchmedo
;; TODO re-use the buffer if it exists already
;; TODO or just pipe from the terminal to a file and open/clear that file in emacs
;; maybe that's the logging experience i've been looking for

(comment
 (run-shell-command-in-background "myspecialbuffer" "echo 'hi there'")
 (run-shell-command-in-background "myspecialbuffer" "pwd")


 (let ((default-directory "~/russmatney"))
   (run-shell-command-in-background "russ" "echo 'hi russ' && pwd"))

 (let ((default-directory "~/urbint/grid/test"))
   (run-shell-command-in-background
    "watchmedo"
    "nix-shell --run 'watchmedo auto-restart --directory . -- echo hi'"))

 (let ((default-directory "~/urbint/grid/test"))
   (run-shell-command-in-background
    "watchmedo"
    "watchmedo auto-restart --patterns='*.py' --ignore-patterns='*flycheck*;*migration*;*yapf*' --recursive --directory ../python/urbint_lib  --directory . --directory ../backend/src -- pytest -s api_tests/ -k 'actions_test and test_api_list_sort' --maxfail=1 --disable-warnings"))

 )


(require 'hydra)

(defhydra hydra-sticky ()
  "Commands that stick around (this hydra supports multiple presses).
"
  ("o" text-scale-decrease "out")
  ("i" text-scale-increase "in"))

(defhydra hydra-main (:exit t)
  "All your hydra are belong to us.
"
  ("g" gdscript-hydra-show "Godot Script Hydra")
  ("s" hydra-sticky/body "Sticky hydra")
  )
