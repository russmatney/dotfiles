;; named `run-init` rather than `init` to prevent accidental lua module loading

(local view (require :fennelview))
(local inspect (require :inspect))
(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(local naughty (require "naughty"))
(local beautiful (require "beautiful"))
(local wibox (require "wibox"))

(local lain (require "lain"))

;; TODO remove these
(require "awful.autofocus")
(require "awful.hotkeys_popup.keys.vim")

;; TODO wrap side effects of these requires
;; REPL env
(require "./remote")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global pp (fn [x] (print (view x))))
(global ppi (fn [x] (print (inspect x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load init globals
(require :config)
(require :bar)

;; create function
(global
 init
 (fn [config]
   ;; error handling
   (print "\n\ninit_error_handling\n")
   (_G.init_error_handling config)

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
   (print "\n\nset_rules\n")
   (_G.set_rules config)

   ;; signals
   (print "\n\ninit_signals\n")
   (_G.init_manage_signal config)
   (_G.init_request_titlebars config)
   (_G.init_focus_signals config)
   (_G.init_arrange_signal config)

   ;; spawns
   (print "\n\ninit_spawns\n")
   (_G.init_spawns config)))

;; hand off to ralphie
;; (awful.spawn "ralphie awesome-init")
;; or call it yourself
(init)
