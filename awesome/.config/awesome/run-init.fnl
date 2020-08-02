;; named `run-init` rather than `init` to prevent accidental lua module loading

(pcall "require" "luarocks.loader")

(local view (require :fennelview))
(local inspect (require :inspect))
(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(local naughty (require "naughty"))
(local beautiful (require "beautiful"))

(local wibox (require "wibox"))
(local gears (require "gears"))
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
   (print "\n\ninit_theme\n")
   (_G.init_theme config)
   (print "\n\ninit_error_handling\n")
   (_G.init_error_handling config)
   (print "\n\ninit_tags\n")
   (_G.init_tags config)
   (print "\n\ninit_screen")
   (_G.init_screen config)
   (print "\n\nset_rules\n")
   (_G.set_rules config)
   (print "\n\ninit_manage_signal\n")
   (_G.init_manage_signal config)
   (print "\n\ninit_request_titlebars\n")
   (_G.init_request_titlebars config)
   (print "\n\ninit_focus_signals\n")
   (_G.init_focus_signals config)
   (print "\n\ninit_arrange_signal\n")
   (_G.init_arrange_signal config)
   (print "\n\ninit_spawns\n")
   (_G.init_spawns config)))

;; hand off to ralphie
(awful.spawn "ralphie awesome-init")

;; TODO remove once it works!
(init)
