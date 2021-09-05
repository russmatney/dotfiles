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
    "watchmedo auto-restart --patterns='*.py' --ignore-patterns='*flycheck*;*migration*;*yapf*' --recursive --directory ../python/urbint_lib  --directory . --directory ../backend/src -- pytest -s api_tests/ -k 'actions_test and test_api_list_sort' --maxfail=1 --disable-warnings")))


(defun russ/reload-emacs-font ()
  "Duplicated font defs from init.el.

For whatever reason changing the font-size seems to change fonts.... this resets
that, but also resets the size..."
  (interactive)
  (setq doom-font (font-spec :family "RobotoMono Nerd Font" :size 20 :slant 'normal)
        doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :slant 'normal)
        doom-unicode-font (font-spec :family "DejaVuSansMono Nerd Font Mono")
        doom-big-font (font-spec :family "SpaceMono Nerd Font" :size 24 :slant 'normal))
  (doom/reload-font))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hydra)

(defhydra hydra-sticky ()
  "
Commands that stick around (this hydra supports multiple presses).
"
  ("o" text-scale-decrease "out (this window)" :column "Zoom/Scale")
  ("i" text-scale-increase "in (this window)")
  ("O" doom/decrease-font-size "Out (all windows)")
  ("I" doom/increase-font-size "In (all windows)")
  ("r" russ/reload-emacs-font "Reset emacs fonts")
  ("e" flycheck-list-errors "list errors" :column "Flycheck")
  ("n" flycheck-next-error "next error")
  ("p" flycheck-previous-error "previous error")
  )

(defhydra hydra-workspace-garden-files (:exit t)
  ("g" hydra-workspace-garden-files/body "workspace garden file"
   :column "Garden Files")

  ("c" (find-file "~/todo/garden/workspaces/clawe.org") "clawe workspace garden file")
  ("d" (find-file "~/todo/garden/workspaces/doctor.org") "doctor workspace garden file")
  )

(defhydra hydra-clawe (:exit t)
  ("b" (find-file "~/russmatney/clawe/src/clawe/defs/bindings.clj")
   "defs/bindings.clj" :column "Open file")
  ("w" (find-file "~/russmatney/clawe/src/clawe/defs/workspaces.clj")
   "defs/workspaces.clj")

  ("R" (shell-command
        "cd ~/russmatney/clawe && bb -cp $(clojure -Spath) --uberjar clawe.jar -m clawe.core")
   "rebuild" :column "clawe mgmt")
  ("r" (shell-command "clawe reload") "reload")
  ;; NOTE this doesn't do the caching/restore that mod+shift+r does yet
  ("A" (shell-command "awesome-client \"awesome.restart()\"") "restart awesome")

  ;; can i expand this hydra into here? compose it?
  ("g" hydra-workspace-garden-files/body "workspace garden file" :column "Garden Files")
  ("c" (find-file "~/todo/garden/workspaces/clawe.org") "clawe workspace garden file")
  ("d" (find-file "~/todo/garden/workspaces/doctor.org") "doctor workspace garden file"))

(defhydra hydra-clojure (:exit t)
  ("'"  cider-jack-in "jack-in (clj)" :column "jack in!")
  ("\"" cider-jack-in-cljs "jack-in (cljs)")
  ("B" cider-switch-to-repl-buffer "Switch to repl buffer" :column "Open buffer")
  ("f" clojure-essential-ref "Open in Clojure, The Essential Reference")
  ("t" cider-test-run-ns-tests "Run tests for namespace" :column "Run tests")
  ("T" cider-test-run-test "Run THIS test")
  ("m" clojure-move-to-let "Move" :column "Refactoring")
  (">" cljr-thread-first-all "Thread first all")
  ("<" cljr-thread-last-all "Thread last all"))

(defhydra hydra-visit-bookmark (:exit t)
  ("t" russ/open-org-file "Open ~/todo org file" :column "~/todo")
  ("p" (find-file "~/todo/projects.org") "Open ~/todo/projects.org")
  ("j" (find-file "~/todo/journal.org") "Open ~/todo/journal.org")
  ("a" org-agenda "Org Agenda")

  ("." russ/open-dotfile "~/dotfiles/*" :column "dotfiles")
  ("z" (find-file "~/.zshrc") "~/.zshrc")
  ("T" (find-file "~/.tmux.conf") "~/.tmux.conf")

  ("d" russ/open-doom-file "Open DOOM source file" :column "emacs")
  ("c" russ/open-emacs-config-file "Open DOOM config file ~/.doom.d/")
  ("b" (find-file "~/.doom.d/+bindings.el") "Open emacs +bindings.el")
  ("h" (find-file "~/.doom.d/+hydra.el") "Open emacs +hydra.el")

  ("B" (find-file "~/russmatney/clawe/src/clawe/defs/bindings.clj")
   "defs/bindings.clj" :column "clawe")
  ("W" (find-file "~/russmatney/clawe/src/clawe/defs/workspaces.clj")
   "defs/workspaces.clj")

  ("w" russ/open-writing-file "Open file in ~/Dropbox/Writing/" :column "Misc")

  ;; TODO `g` for opening the current workspace's garden note
  ;; TODO `r` for opening the current workspace's readme
  )

(defhydra hydra-emacs-help (:exit t)
  ("r" russ/reload-emacs-font "Reload emacs fonts")
  )

(defhydra hydra-main (:exit t)
  ("g" gdscript-hydra-show "Godot Script Hydra"
   :column "All your Hydra are belong to us")
  ("s" hydra-sticky/body "Sticky hydra")
  ("c" hydra-clojure/body "Clojure hydra")
  ("C" hydra-clawe/body "Clawe hydra")
  ("e" hydra-visit-bookmark/body "Visit/Edit/Bookmarks")
  ("r" hydra-org-refile/body "Org refiling")

  ("h" hydra-emacs-help/body "Emacs help")

  ;; TODO create an evil-ex for this as well
  ("a" counsel-projectile-rg "Projectile project search"))
