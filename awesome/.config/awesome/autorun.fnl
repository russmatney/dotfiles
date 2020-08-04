(local awful (require :awful))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_spawns
 (fn []
   ;; spawn autorun
   (awful.spawn.once "~/.config/awesome/autorun.sh")

   ;; restart some app services
   (awful.spawn.once "run sc --user restart yodo yodo-fe bb-nrepl ralphie-nrepl")
   (awful.spawn "xset r rate 150 60")

   ;; kick variety to fix the background asap
   ;; TODO write the current/latest to the current theme
   (awful.spawn "variety --next")))
