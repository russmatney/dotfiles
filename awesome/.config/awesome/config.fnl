(pcall "require" "luarocks.loader")

(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")
(local naughty (require "naughty"))
(local wibox (require "wibox"))
(require "awful.hotkeys_popup.keys.vim")
(local beautiful (require "beautiful"))

(local view (require :fennelview))
(global pp (fn [x] (print (view x))))

(require "./remote")

(local w (require :workspaces))
(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (beautiful.init "/usr/share/awesome/themes/default/theme.lua")
(beautiful.init "/usr/share/awesome/themes/cesious/theme.lua")
;; (beautiful.init "/usr/share/awesome/themes/gtk/theme.lua")
;; (beautiful.init "/usr/share/awesome/themes/sky/theme.lua")
;; (beautiful.init "/usr/share/awesome/themes/xresources/theme.lua")
;; (beautiful.init "/usr/share/awesome/themes/zenburn/theme.lua")
;; (beautiful.init "./theme.lua")
(set beautiful.icon_theme "Papirus-Dark")
(set beautiful.bg_normal "#141A1B")
(set beautiful.bg_focus "#222B2E")
(set beautiful.font "Noto Sans Regular 10")
(set beautiful.notification_font "Noto Sans Bold 14")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if awesome encountered an error during startup and fell back to
;; another config (This code will only ever execute for the fallback config)
(if _G.awesome.startup_errors
    (naughty.notify {:preset  naughty.config.presets.critical
                     :title  "Oops, there were errors during startup!"
                     :text  _G.awesome.startup_errors }))

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
;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local modifiers {:mod "Mod4"
                  :shift "Shift"
                  :ctrl "Control"})

(fn map-mods
  [mods]
  (->> mods
       (fun.map (partial . modifiers))
       (fun.totable)))

(fn btn
  [mods btn-code fun]
  (awful.button (map-mods mods) btn-code fun))

(fn add-all-tags []
  (awful.tag w.tag-names
             (awful.screen.focused)
             awful.layout.suit.tile))

;; TODO create an init?
(add-all-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIBAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create a textclock widget
(local mytextclock (wibox.widget.textclock "%H:%M "))

(local blue "#9EBABA")
(local separator (wibox.widget.textbox
                  (.. "         <span color=\""
                      blue
                      "\">| </span>         ")))

;; Create a wibox for each screen and add it
(local taglist_buttons
       (gears.table.join
        (btn [] 1 (fn [t] (t:view_only)))
        (btn [:mod] 1
             (fn [t]
               (if _G.client.focus
                   (_G.client.focus:move_to_tag t))))
        (btn [] 3 awful.tag.viewtoggle)
        (btn [:mod] 3
             (fn [t]
               (if _G.client.focus
                   (_G.client.focus:toggle_tag t))))
        (btn [] 4 (fn [t] (awful.tag.viewnext t.screen)))
        (btn [] 5 (fn [t] (awful.tag.viewprev t.screen)))))

(local tasklist_buttons
       (gears.table.join
        (btn []  1
             (fn [c]
               (if
                (= c _G.client.focus)
                (tset c :minimized true))

               (do
                 ;; Without this, the following
                 ;; :isvisible() makes no sense
                 (tset c :minimized false)

                 (when (and (not (c:isvisible)) c.first_tag)
                   (c.first_tag:view_only))

                 ;; This will also un-minimize
                 ;; the client, if needed
                 (tset _G.client :focus c)
                 (c:raise))))

        ;; TODO fill in global right click? maybe hit ralphie?
        ;; awful.button({ }, 3, client_menu_toggle_fn()),
        (btn [] 4 (fn [] (awful.client.focus.byidx 1)))
        (btn [] 5 (fn [] (awful.client.focus.byidx -1)))))

(awful.screen.connect_for_each_screen
 (fn [s]
   ;; Create a promptbox for each screen
   (set s.mypromptbox (awful.widget.prompt))
   ;; Create an imagebox widget which will contains an icon indicating which layout we're using.
   ;; We need one layoutbox per screen.
   (set s.mylayoutbox (awful.widget.layoutbox s))

   (s.mylayoutbox:buttons
    (gears.table.join
     (btn [] 1 (fn [] (awful.layout.inc 1)))
     (btn [] 3 (fn [] (awful.layout.inc -1)))
     (btn [] 4 (fn [] (awful.layout.inc 1)))
     (btn [] 5 (fn [] (awful.layout.inc -1)))))

   ;; Create a taglist widget
   (set s.mytaglist
        (awful.widget.taglist
         {:screen s
          :filter awful.widget.taglist.filter.all
          :buttons taglist_buttons
          :update_function awful.widget.common.list_update}))

   ;; Create a tasklist widget
   (set s.mytasklist
        (awful.widget.tasklist
         {:screen s
          :filter awful.widget.tasklist.filter.currenttags
          :buttons tasklist_buttons}))

   ;; Create the wibox
   (set s.mywibox
        (awful.wibar {:position "top" :screen s}))

   ;; Add widgets to the wibox
   (s.mywibox:setup
    {:layout wibox.layout.align.horizontal
     1  {:layout wibox.layout.fixed.horizontal ;; Left widgets
         1 s.mytaglist
         2 s.mypromptbox
         3 separator}

     ;; Middle widget
     2 s.mytasklist

     ;; Right widgets
     3 {:layout wibox.layout.fixed.horizontal
        1 (wibox.widget.systray)
        2 separator
        3 mytextclock
        4 s.mylayoutbox}})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var assigned-browser false)

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
       ;; {:rule {:role "browser"}
       ;;  :properties {:screen 1}
       ;;  :callback
       ;;  (fn [c]
       ;;    (if (not assigned-browser)
       ;;        (let [tag (awful.tag.find_by_name
       ;;                   (awful.screen.focused)
       ;;                   w.web-tag.tag-name)]
       ;;          (tset c :above true)
       ;;          (tset c :floating true)
       ;;          (when tag
       ;;            (awful.client.movetotag tag c))
       ;;          (set assigned-browser true))))}
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
       (awful.placement.no_offscreen c))))

;; Add a titlebar if titlebars_enabled is set to true in the rules.
(_G.client.connect_signal
 "request::titlebars"
 (fn [c]
   (let [buttons
         (gears.table.join
          (btn [] 1 (fn []
                      (set client.focus c)
                      (c:raise)
                      (awful.mouse.client.move c)))
          (btn [] 3 (fn []
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
