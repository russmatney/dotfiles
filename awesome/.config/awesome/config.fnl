(pcall "require" "luarocks.loader")

(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")
(require "awful.hotkeys_popup.keys.vim")

(local ralphie (require "ralphie"))
(local naughty (require "naughty"))
(local wibox (require "wibox"))

(local hotkeys_popup (require "awful.hotkeys_popup"))
(local beautiful (require "beautiful"))

(require "./remote")

(local view (require :fennelview))
(global pp (fn [x] (print (view x))))

(local w (require :workspaces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(beautiful.init "/usr/share/awesome/themes/cesious/theme.lua")
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

(fn notify
  [text]
  (naughty.notify {:text text}))

(fn debug
  [x]
  (notify (gears.debug.dump_return x)))

(local modifiers {:mod "Mod4"
                  :shift "Shift"
                  :ctrl "Control"})

(fn map-mods
  [mods]
  (->> mods
       (fun.map (partial . modifiers))
       (fun.totable)))

(fn key
  [mods key-code fun opt]
  (let [opt (or opt {})]
    (awful.key (map-mods mods) key-code fun opt)))

(fn btn
  [mods btn-code fun]
  (awful.button (map-mods mods) btn-code fun))

(fn spawn-fn
  [cmd]
  (fn []
    (awful.spawn_with_shell cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn add-all-tags []
  (awful.tag w.tag-names (awful.screen.focused) awful.layout.suit.tile))

(add-all-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create Client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO refactor into tags/workspaces/clients datastructure (frame-name, filename, tag name)
(lambda create-client
  [workspace]
  (let [emacs-file (. workspace :emacs-file)
        browser-url (. workspace :browser-url)]
    (if
     browser-url
     (awful.spawn
      (.. "google-chrome-stable --new-window " browser-url))

     emacs-file
     (awful.spawn.with_shell
      (.. "emacsclient --alternate-editor='' --no-wait --create-frame "
          emacs-file
          " -F '(quote (name . \""
          (. workspace :tag-name)
          "\"))' --display $DISPLAY")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle Scratchpad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn toggle-scratchpad
  [workspace]
  (fn []
    (let [tag-name (. workspace :tag-name)

          s (awful.screen.focused)
          x-tag (awful.tag.find_by_name s tag-name)
          x-client (when x-tag
                     (when (> (length (x-tag:clients)) 0)
                       (-> (x-tag:clients)
                           (. 1))))]
      (if
       ;; if tag and a client, toggle tag, focus client
       (and x-tag x-client)
       (do
         (awful.tag.viewtoggle x-tag)
         (tset x-client :ontop (not (. x-client :ontop)))
         (if (not x-client.active)
             ;; _G indicates a 'true' global, that fennel did not reject
             (tset _G.client :focus x-client)))

       ;; if tag but no client, create client
       (and x-tag (not x-client))
       (create-client workspace)

       ;; no tag? create it
       (not x-tag)
       (do
         (awful.tag.add tag-name {:screen s
                                  :gap 10})
         ;; TODO should only create here if no x-client exists
         ;; across whole system, not just in this tag
         (create-client workspace))
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIBAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create a textclock widget
(local mytextclock (wibox.widget.textclock "%H:%M "))

(local blue "#9EBABA")
(local red "#EB8F8F")
(local separator (wibox.widget.textbox
                  (.. "         <span color=\"" blue "\">| </span>         ")))

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
         s awful.widget.taglist.filter.all taglist_buttons))

   ;; Create a tasklist widget
   (set s.mytasklist
        (awful.widget.tasklist
         s awful.widget.tasklist.filter.currenttags tasklist_buttons))

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
;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local global-keys
       (gears.table.join
        ;; helpers
        ;; TODO syntax check/verify on config files before restarting
        (key [:mod :shift] "r" _G.awesome.restart)
        ;; (key [:mod :shift] "?" hotkeys_popup.widget.show_help)

        ;; ralphie rofi
        (key [:mod] "x" (spawn-fn "ralphie rofi"))

        ;; walk tags
        (key [:mod] "Left" awful.tag.viewprev)
        (key [:mod] "Right" awful.tag.viewnext)

        ;; previous tag
        (key [:mod] "Escape" awful.tag.history.restore)

        ;; scratchpads
        (key [:mod] "u" (toggle-scratchpad w.journal-tag))
        (key [:mod] "y" (toggle-scratchpad w.yodo-tag))
        (key [:mod] "r" (toggle-scratchpad w.notes-tag))
        (key [:mod] "t" (toggle-scratchpad w.web-tag))
        (key [:mod] "0" (toggle-scratchpad w.dotfiles-tag))

        ;; cycle clients
        (key [:mod] "Tab"
             (fn []
               (awful.client.focus.byidx 1)
               (when _G.client.focus
                 (_G.client.focus:raise))))
        (key [:mod :shift] "Tab"
             (fn []
               (awful.client.focus.byidx -1)
               (when _G.client.focus
                 (_G.client.focus:raise))))

        ;; terminal
        (key [:mod] "Return" (spawn-fn "ralphie open-term"))

        ;; emacs
        (key [:mod :shift] "Return" (spawn-fn "ralphie open-emacs"))

        ;; browser
        (key [:mod :shift] "b" (spawn-fn "google-chrome-stable"))

        ;; launcher (rofi)
        (key [:mod] "space" (spawn-fn "/usr/bin/rofi -show drun -modi drun"))

        ;; finder (thunar)
        (key [:mod] "e" (spawn-fn "/usr/bin/thunar"))

        ;; widen/shink window
        (key [:mod :shift] "l" (fn [] (awful.tag.incmwfact 0.05)))
        (key [:mod :shift] "h" (fn [] (awful.tag.incmwfact -0.05)))
        (key [:mod :shift] "j" (fn [] (awful.client.incwfact 0.05)))
        (key [:mod :shift] "k" (fn [] (awful.client.incwfact -0.05)))

        ;; restore minimized
        (key [:mod :shift] "n" (fn [] (let [c (awful.client.restore)]
                                        (when c
                                          (tset _G.client :focus c)
                                          (c:raise)))))

        ;; screenshots
        (key [:mod :shift] "s" (spawn-fn "/usr/bin/i3-scrot"))
        (key [:mod :shift] "a" (spawn-fn "/home/russ/.local/bin/screenshot-region"))

        ;; media controls
        (key [] "XF86AudioPlay" (spawn-fn "spotifycli --playpause"))
        (key [] "XF86AudioNext" (spawn-fn "playerctl next"))
        (key [] "XF86AudioPrev" (spawn-fn "playerctl previous"))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbered Tag global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var tag-keys [])

(for [it 0 9]
  (set tag-keys
       (gears.table.join
        tag-keys

        ;; show tag (workspace)
        (key [:mod] (.. "#" (+ 9 it))
             (fn []
               (let [scr (awful.screen.focused)
                     keyed-tag (. scr.tags it)
                     current-tag scr.selected_tag]
                 (when keyed-tag
                   (if (and current-tag (= keyed-tag.name current-tag.name))
                       (awful.tag.history.restore scr 1)
                       (keyed-tag:view_only))))))

        ;; add tag to current perspective
        (key [:mod :ctrl] (.. "#" (+ 9 it))
             (fn []
               (let [scr (awful.screen.focused)
                     scr-tag (. scr.tags it)]
                 (when scr-tag (awful.tag.viewtoggle scr-tag)))))

        ;; move current focus to tag (workspace)
        (key [:mod :shift] (.. "#" (+ 9 it))
             (fn []
               (when _G.client.focus
                 (let [scr-tag (. _G.client.focus.screen.tags it)]
                   (when scr-tag
                     (_G.client.focus:move_to_tag scr-tag))))))

        ;; add/remove focused client on tag
        (key [:mod :shift :ctrl] (.. "#" (+ 9 it))
             (fn []
               (when _G.client.focus
                 (let [scr-tag (. _G.client.focus.screen.tags it)]
                   (when scr-tag
                     (_G.client.focus:toggle_tag scr-tag)))))))))


(_G.root.keys (gears.table.join global-keys tag-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local clientkeys
       (gears.table.join
        ;; kill current client
        (key [:mod] "q" (fn [c] (c:kill)))

        ;; toggle floating
        (key [:mod] "f" awful.client.floating.toggle)

        ;; toggle keep-on-top
        ;; (key [:mod] "t" (fn [c] (tset c :ontop (not c.ontop))))

        ;; center on screen
        (key [:mod] "c" (fn [c]
                          ((+ awful.placement.scale
                              awful.placement.centered)
                           c
                           {:to_percent 0.75})))
        ;; center on screen
        (key [:mod :shift] "c" (fn [c]
                                 ((+ awful.placement.scale
                                     awful.placement.centered)
                                  c
                                  {:to_percent 0.9})))

        ;; swap with master
        (key [:mod :ctrl] "Return" (fn [c] (c:swap (awful.client.getmaster))))

        ;; minimize
        (key [:mod] "n" (fn [c] (tset c :minimized true)))

        ;; toggle full-screen
        (key [:mod] "m" (fn [c]
                          (tset c :maximized (not c.maximized))
                          (c:raise)))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_G.root.buttons (gears.table.join
                  ;; (btn [] 1 mymainmenu:hide)
                  ;; (btn [] 3 mymainmenu:toggle)
                  (btn [] 4 awful.tag.viewnext)
                  (btn [] 5 awful.tag.viewprev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local clientbuttons
       (gears.table.join
        (btn [] 1 (fn [c]
                    (tset _G.client :focus c)
                    (c:raise)))
        (btn [:mod] 1 awful.mouse.client.move)
        (btn [:mod] 3 awful.mouse.client.resize)))

(var assigned-browser false)

;; Rules to apply to new clients (through the "manage" signal).
(tset awful.rules
      :rules
      (gears.table.join
       {:rule {}
        :properties {:border_width beautiful.border_width
                     :border_color beautiful.border_normal
                     :focus awful.client.focus.filter
                     :raise true
                     :keys clientkeys
                     :buttons clientbuttons
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
       {:rule {:role "browser"}
        :properties {:screen 1}
        :callback
        (fn [c]
          (if (not assigned-browser)
              (let [tag (awful.tag.find_by_name
                         (awful.screen.focused)
                         w.web-tag.tag-name)]
                (tset c :above true)
                (tset c :floating true)
                (when tag
                  (awful.client.movetotag tag c))
                (set assigned-browser true))))}

       ;; convert to function with slack-tag arg
       {:rule_any {:class ["Slack" "slack" "discord"]
                   :name ["slack" "Slack"]}
        :properties {:tag w.slack-tag.tag-name
                     :floating false}}

       {:rule_any {:class ["spotify" "pavucontrol"]
                   :name ["spotify" "Spotify" "pavucontrol" "Pavucontrol"]}
        :properties {:tag w.spotify-tag.tag-name
                     :floating false}}

       w.rules-scratchpad-emacs
       ))

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
(awful.spawn.with_shell "~/.config/awesome/autorun.sh")
