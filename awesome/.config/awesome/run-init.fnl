;; named `run-init` rather than `init` to prevent accidental lua module loading

(local awful (require "awful"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load global init functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :config)
(require :bar)
(require :remote)
(require :rules)
(require :signals)
(require :titlebars)
(require :autorun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init
 (fn [config]
   ;; error handling
   (print "\n\ninit_error_handling\n")
   (_G.init_error_handling config)

   ;; init remote
   (print "\n\ninit_remote\n")
   (_G.init_remote config)

   ;; theme
   (print "\n\ninit_theme\n")
   (_G.init_theme config)

   ;; screen and tags
   (print "\n\ninit screen and tags")
   (_G.init_screen config)
   (_G.init_tags config)

   ;; bindings
   (print "\n\ninit bindings\n")
   (_G.set_global_keys config)
   (_G.init_root_buttons config)

   ;; rules
   (print "\n\ninit_rules\n")
   (_G.init_rules config)

   ;; signals
   (print "\n\ninit_signals\n")
   (_G.init_manage_signal config)
   (_G.init_request_titlebars config)
   (_G.init_focus_signals config)
   (_G.init_arrange_signal config)

   ;; spawns
   (print "\n\ninit_spawns\n")
   (_G.init_spawns config)))

;; (awful.spawn "ralphie awesome-init")
(init)
