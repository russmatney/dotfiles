(local awful (require :awful))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_spawns
 (fn []
   ;; spawn autorun
   (awful.spawn "~/.config/awesome/autorun.sh" false)

   ;; startup some app services
   (awful.spawn.once "run sc --user restart yodo yodo-fe bb-nrepl ralphie-nrepl" false)
   (awful.spawn "xset r rate 170 60" false)
   (awful.spawn.once "picom" false)

   ;; kick variety to fix the background asap
   ;; TODO write the current/latest to the current theme
   (awful.spawn "variety --next" false)

   (awful.spawn "ralphie start-bar")
   ))
