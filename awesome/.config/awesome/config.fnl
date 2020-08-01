(pcall "require" "luarocks.loader")

(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")
(local naughty (require "naughty"))
(require "awful.hotkeys_popup.keys.vim")
(local beautiful (require "beautiful"))

(local wibox (require "wibox"))
(local gears (require "gears"))
(local awful (require "awful"))

(local lain (require "lain"))

(local view (require :fennelview))
(local inspect (require :inspect))
(global pp (fn [x] (print (view x))))
(global ppi (fn [x] (print (inspect x))))

;; REPL env
(require "./remote")

;; top/bottom bar
(require "./bar")

(local w (require :workspaces))
(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init-theme
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

;; TODO move to global init
(init-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if awesome encountered an error during startup and fell back to
;; another config (This code will only ever execute for the fallback config)
(if _G.awesome.startup_errors
    (naughty.notify {:preset  naughty.config.presets.critical
                     :title  "Oops, there were errors during startup!"
                     :text  _G.awesome.startup_errors }))

;; TODO fix the p.o.s. default config's bg, bindings, etc
;; TODO fix error handling ux

;; Handle runtime errors after startup
(let []
  (var in_error false)
  (_G.awesome.connect_signal
   "debug::error"
   (fn [err]
     ;; Make sure we don't go into an endless error loop
     (if (not in_error)
         (set in_error true)
         (naughty.notify {:preset naughty.config.presets.critical
                          :title "Oops, an error happened!"
                          :text (tostring err) })
         (set in_error false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 add_all_tags
 (fn []
   (awful.tag w.tag-names
              (awful.screen.focused)
              lain.layout.centerwork)))

;; ;; TODO create an init?
(add_all_tags)

(global
 reapply_rules
 (fn []
   (each [c (awful.client.iterate (fn [_] true))]
     (awful.rules.apply c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rules to apply to new clients (through the "manage" signal).
(set awful.rules.rules
     (gears.table.join
      [{:rule {}
        :properties
        {:border_width beautiful.border_width
         :border_color beautiful.border_normal
         :focus awful.client.focus.filter
         :raise true
         :keys bindings.clientkeys
         :buttons bindings.clientbuttons
         :size_hints_honor false ;; Remove gaps between terminals
         :screen awful.screen.preferred
         :callback awful.client.setslave
         :placement (+ awful.placement.no_overlap
                       awful.placement.no_offscreen)}}

       ;; {:rule {}
       ;;  :callback
       ;;  (fn [c]
       ;;    (print "\n\nnew client!")
       ;;    (pp c)
       ;;    (ppi c)
       ;;    (print c.class)
       ;;    (print c.name)
       ;;    )}

       ;; Floating clients.
       {:rule_any
        {:instance ["DTA" "copyq"]
         :class ["Arandr" "MessageWin" "Sxiv" "Wpa_gui"
                 "pinentry" "veromix" "xtightvncviewer"]
         :role ["pop-up"]} ;; e.g. Google Chrome's (detached) Developer Tools.
        :properties {:floating true}}

       ;; Add titlebars to normal clients and dialogs
       {:rule_any {:type ["normal" "dialog"]}
        :properties {:titlebars_enabled true}}

       ;; attempt to wrangle browser windows
       ;; currently too broad, catching slack, discord, spotify
       {:rule {:role "browser"}
        :properties {:screen 1}
        :callback
        (fn [c]
          (if
           (and
            (not (= c.class "Slack"))
            (not c.name_change_handled))
           (do
             (var f nil)
             (set f
                  (fn [c]
                    (tset c :name_change_handled true)
                    (c:disconnect_signal "property::name" f)
                    (awful.rules.apply c)
                    (set c.minimized false)
                    ))
             (set c.minimized true)
             (c:connect_signal "property::name" f))))}
       ]

      w.rules-scratchpad-emacs
      w.rules-apps-on-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signals

;; Signal function to execute when a new client appears.
(_G.client.connect_signal
 "manage"
 (fn [c]
   ;; Set the windows at the slave,
   ;; i.e. put it at the end of others instead of setting it master.
   ;; if not awesome.startup then awful.client.setslave(c) end
   (if (and _G.awesome.startup
            (not c.size_hints.user_position)
            (not c.size_hints.program_position))
       ;; Prevent clients from being unreachable after screen count changes.
       (awful.placement.no_offscreen c))

   (if (not c.class)
       (do
         (set c.minimized true)
         (c:connect_signal
          "property::class"
          (fn [c]
            (set c.minimized false)
            (awful.rules.apply c)))))))

;; Add a titlebar if titlebars_enabled is set to true in the rules.
(_G.client.connect_signal
 "request::titlebars"
 (fn [c]
   (let [buttons
         (gears.table.join
          (bindings.btn [] 1 (fn []
                               (set client.focus c)
                               (c:raise)
                               (awful.mouse.client.move c)))
          (bindings.btn [] 3 (fn []
                               (set client.focus c)
                               (c:raise)
                               (awful.mouse.client.resize c))))

         titlebar (awful.titlebar c)]

     (titlebar:setup
      {1 { ;; Left
          1 (awful.titlebar.widget.iconwidget c)
          :buttons buttons
          :layout  wibox.layout.fixed.horizontal}
       2 {  ;; Middle
          1 { ;; Title
             :align  "center"
             :widget (awful.titlebar.widget.titlewidget c)}
          :buttons buttons
          :layout  wibox.layout.flex.horizontal}
       3 { ;; Right
          1 (awful.titlebar.widget.floatingbutton c)
          2 (awful.titlebar.widget.stickybutton   c)
          3 (awful.titlebar.widget.ontopbutton    c)
          4 (awful.titlebar.widget.maximizedbutton c)
          5 (awful.titlebar.widget.closebutton    c)
          :layout (wibox.layout.fixed.horizontal)
          }
       :layout wibox.layout.align.horizontal}))))

(_G.client.connect_signal
 "focus"
 (fn [c] (set c.border_color beautiful.border_focus)))

(_G.client.connect_signal
 "unfocus"
 (fn [c] (set c.border_color beautiful.border_normal)))

;; Disable borders on lone windows
;; Handle border sizes of clients.
(for [s 1 (_G.screen.count)]
  (let [sc (. _G.screen s)]
    (sc:connect_signal
     "arrange"
     (fn []
       (local clients (awful.client.visible s))
       (local layout (awful.layout.getname (awful.layout.get s)))

       (each [_ c (pairs clients)]
         ;; No borders with only one humanly visible client
         (if c.maximized
             ;; NOTE: also handled in focus, but that does not cover maximizing from a
             ;; tiled state (when the client had focus).
             (set c.border_width 0)

             (or c.floating (= layout "floating"))
             (set c.border_width beautiful.border_width)

             (or (=  layout "max")  (= layout "fullscreen"))
             (set c.border_width 0)

             (do
               (local tiled (awful.client.tiled c.screen))
               (if (= #tiled 1) ;; and c = tiled[1] then
                   (tset (. tiled 1) :border_width 0)
                   ;; if layout ~= "max" and layout ~= "fullscreen" then
                   ;; XXX: SLOW!
                   ;; awful.client.moveresize(0, 0, 2, 0, tiled[1])
                   ;; end
                   (tset c :border_width beautiful.border_width)))))))))

;; spawn autorun
(awful.spawn.once "~/.config/awesome/autorun.sh")

;; restart some app services
(awful.spawn.once "run sc --user restart yodo yodo-fe bb-nrepl ralphie-nrepl")
(awful.spawn "xset r rate 150 60")

;; kick variety to fix the background asap
;; TODO write the current/latest to the current theme
(awful.spawn "variety --next")
