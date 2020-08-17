;; named `run-init` rather than `init` to prevent accidental lua module loading

(local awful (require "awful"))
(local beautiful (require "beautiful"))
(local view (require :fennelview))
(local inspect (require :inspect))
(local lain (require "lain"))

;; focus client after awesome.restart
(require "awful.autofocus")
(require "./helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global pp (fn [x] (print (view x))))
(global ppi (fn [x] (print (inspect x))))

(global layouts
        [awful.layout.suit.tile
         ;; awful.layout.suit.floating
         ;; awful.layout.suit.fair
         ;; awful.layout.suit.magnifier
         ;; awful.layout.suit.spiral
         ;; awful.layout.suit.spiral.dwindle
         lain.layout.centerwork
         ;; lain.layout.centerwork.horizontal
         ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local restart-helper (require "./restart"))

(awesome.connect_signal
 "startup"
 (fn [] (restart-helper.restore_state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_theme
 (fn []
   ;; init theme
   (-> (require "gears.filesystem")
       (. :get_configuration_dir)
       ((fn [f] (f)))
       (.. "theme/theme.lua")
       beautiful.init)
   (set beautiful.icon_theme "Papirus-Dark")
   (set beautiful.bg_normal "#141A1B")
   (set beautiful.bg_focus "#222B2E")
   (set beautiful.font "Noto Sans Regular 10")
   (set beautiful.notification_font "Noto Sans Bold 14")
   (set beautiful.useless_gap 30)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load global init functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :config)
(require :bar)
(require :remote)
(require :rules)
(require :signals)
(require :titlebars)
(require :spawns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init
 (fn [config]
   ;; error handling
   (print "init_error_handling")
   (_G.init_error_handling config)

   ;; init remote
   (print "init_remote")
   (_G.init_remote config)

   ;; theme
   (print "init_theme")
   (_G.init_theme config)

   ;; screen and tags
   (print "init screen and tags")
   (_G.init_screen config)
   (_G.init_tags config)

   ;; bindings
   (print "init bindings")
   (_G.set_global_keys config)
   (_G.init_root_buttons config)

   ;; rules
   (print "init_rules")
   (_G.init_rules config)

   ;; signals
   (print "init_signals")
   (_G.init_manage_signal config)
   (_G.init_request_titlebars config)
   (_G.init_focus_signals config)
   (_G.init_arrange_signal config)

   ;; spawns
   (print "init_spawns")
   (_G.init_spawns config)))

;; (awful.spawn "ralphie awesome-init")
(init)
