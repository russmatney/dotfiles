(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")

(local ralphie (require "ralphie"))
(local naughty (require "naughty"))
(local wibox (require "wibox"))

;; (require "./table-serialization")
;; (require "./table-indexof")


(local hotkeys_popup (require "awful.hotkeys_popup"))
(local beautiful (require "beautiful"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Restart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local my_state_file
       nil
       ;; (.. awful.util.get_cache_dir  "~/.tmp-state")
       )

;; (fn save_state []
;;   (local tags (awful.tag.gettags mouse.screen))

;;   (local params {})

;;   (each [i t (ipairs tags)]
;;     (table.insert params [i
;;                           (table.indexofmytags.layout t.layout)
;;                           (awful.tag.getncolt)
;;                           (awful.tag.getmwfactt)
;;                           (awful.tag.getnmastert)]))

;;   (table.save params my_state_file))

;; (fn smart_restart []
;;   (save_state)
;;   (awesome.restart))

;; (fn restore_state []
;;   (when false
;;     ;; when (~= (posix.stat my_state_file) nil)
;;     (local params (table.load my_state_file))
;;     (os.remove my_state_file)

;;     (local s (awful.screen.focused))
;;     (each [j p (ipairs params)]
;;       (local i (. p 1)) ;; index of layout in mytags.layout table
;;       (local l (. p 2))
;;       (local ncol (. p 3))
;;       (local mwfact (. p 4))
;;       (local nmaster (.  p 5))

;;       (local t (. s.tags i))
;;       ;; (t.layout (. mytags.layout l))

;;       (awful.tag.setncol ncol t)
;;       (awful.tag.setmwfact mwfact t)
;;       (awful.tag.setnmaster nmaster t))))

;; (awesome.connect_signal "startup" restore_state)

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
    (awful.spawn cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local journal-tag
       {:tag-name "journal"
        :emacs-file "~/todo/journal.org"})

(local notes-tag
       {:tag-name "notes"
        :emacs-file "~/Dropbox/notes/readme.org"})

(local yodo-tag
       {:tag-name "yodo"
        :browser-url "http://localhost:4200"
        ;; "http://localhost:4222/devcards.html"
        })

(local web-tag
       {:tag-name "web"
        :browser-url "chrome://newtab"})

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
         ;;  TODO if showing, set client ontop
         ;;  TODO if hiding, unset client ontop
         (awful.tag.viewtoggle x-tag)
         (when (not x-client.active)
           (tset client :focus x-client)))

       ;; if tag but no client, create client
       (and x-tag (not x-client))
       (create-client workspace)

       ;; no tag? create it
       (not x-tag)
       (do
         (awful.tag.add tag-name {:screen s})
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
                  (.. "     <span color=\"" blue "\">| </span>    ")))

;; Create a wibox for each screen and add it
(local taglist_buttons
       (gears.table.join
        (btn [] 1 (fn [t] (t:view_only)))
        (btn [:mod] 1
             (fn [t]
               (if client.focus
                   (client.focus:move_to_tag t))))
        (btn [] 3 awful.tag.viewtoggle)
        (btn [:mod] 3
             (fn [t]
               (if client.focus
                   (client.focus:toggle_tag t))))
        (btn [] 4 (fn [t] (awful.tag.viewnext t.screen)))
        (btn [] 5 (fn [t] (awful.tag.viewprev t.screen)))))

(local tasklist_buttons
       (gears.table.join
        (btn []  1
             (fn [c]
               (if
                (= c client.focus)
                (tset c :minimized true))

               (do
                 ;; Without this, the following
                 ;; :isvisible() makes no sense
                 (tset c :minimized false)

                 (when (and (not (c:isvisible)) c.first_tag)
                   (c.first_tag:view_only))

                 ;; This will also un-minimize
                 ;; the client, if needed
                 (tset client :focus c)
                 (c:raise))))

        ;; TODO fill in global right click? maybe hit ralphie?
        ;; awful.button({ }, 3, client_menu_toggle_fn()),
        (btn [] 4 (fn [] (awful.client.focus.byidx 1)))
        (btn [] 5 (fn [] (awful.client.focus.byidx -1)))))

(local set_wallpaper
       (fn [s]
         ;; Wallpaper
         (if beautiful.wallpaper
             (do
               (local wallpaper beautiful.wallpaper)
               ;; If wallpaper is a function, call it with the screen

               (gears.wallpaper.maximized
                (if (= (type wallpaper) "function")
                    (wallpaper s)
                    wallpaper)
                s true)))))

;; Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
(screen.connect_signal "property::geometry" set_wallpaper)

(awful.screen.connect_for_each_screen
 (fn [s]
   ;; Each screen has its own tag table.
   (awful.tag
    ["slack" "spotify" "web" "notes" "awesome" "yodo" "journal"]
    s
    awful.layout.suit.tile)
   ;; [
   ;;  awful.layout.suit.tile
   ;;  awful.layout.suit.floating
   ;;  ;;awful.layout.suit.tile.left
   ;;  awful.layout.suit.tile.bottom
   ;;  ;; awful.layout.suit.tile.top
   ;;  awful.layout.suit.fair
   ;;  awful.layout.suit.fair.horizontal
   ;;  ;; awful.layout.suit.spiral
   ;;  ;; awful.layout.suit.spiral.dwindle
   ;;  awful.layout.suit.max
   ;;  ;; awful.layout.suit.max.fullscreen
   ;;  awful.layout.suit.magnifier
   ;;  ;; awful.layout.suit.corner.nw
   ;;  ;; awful.layout.suit.corner.ne
   ;;  ;; awful.layout.suit.corner.sw
   ;;  ;; awful.layout.suit.corner.se
   ;;  ]

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
        (key [:mod :shift] "r" awesome.restart)
        ;; (key [:mod :shift] "?" hotkeys_popup.widget.show_help)

        ;; ralphie rofi
        (key [:mod] "x" (fn []
                          (ralphie.cmd "rofi")))

        ;; walk tags
        (key [:mod] "Left" awful.tag.viewprev)
        (key [:mod] "Right" awful.tag.viewnext)

        ;; previous tag
        (key [:mod] "Escape" awful.tag.history.restore)

        ;; scratchpads
        (key [:mod] "u" (toggle-scratchpad journal-tag))
        (key [:mod] "y" (toggle-scratchpad yodo-tag))
        (key [:mod] "r" (toggle-scratchpad notes-tag))
        (key [:mod] "t" (toggle-scratchpad web-tag))

        ;; cycle clients
        (key [:mod] "Tab" (fn []
                            (awful.client.focus.history.previous)
                            (when client.focus
                              (client.focus:raise))))

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
                                          (tset client :focus c)
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
                     keyed-tag (. screen.tags it)
                     current-tag screen.selected_tag]
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
               (when client.focus
                 (let [scr-tag (. client.focus.screen.tags it)]
                   (when scr-tag
                     (client.focus:move_to_tag scr-tag))))))

        ;; add/remove focused client on tag
        (key [:mod :shift :ctrl] (.. "#" (+ 9 it))
             (fn []
               (when client.focus
                 (let [scr-tag (. client.focus.screen.tags it)]
                   (when scr-tag
                     (client.focus:toggle_tag scr-tag)))))))))


(root.keys (gears.table.join global-keys tag-keys))


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
                           {:to_percent 0.6})))
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

(root.buttons (gears.table.join
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
                    (tset client :focus c)
                    (c:raise)))
        (btn [:mod] 1 awful.mouse.client.move)
        (btn [:mod] 3 awful.mouse.client.resize)))

(var assigned-browser false)

;; Rules to apply to new clients (through the "manage" signal).
(tset awful.rules
      :rules
      [{:rule {}
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
        {:instance ["DTA" ;; Firefox addon DownThemAll.
                    "copyq"] ;; Includes session name in class.
         :class ["Arandr"
                 "Gpick"
                 "Kruler"
                 "MessageWin" ;; kalarm.
                 "Sxiv"
                 "Wpa_gui"
                 "pinentry"
                 "veromix"
                 "xtightvncviewer"]
         :name ["Event Tester"] ;; xev.
         :role ["AlarmWindow" ;; Thunderbird's calendar.
                "pop-up"]} ;; e.g. Google Chrome's (detached) Developer Tools.
        :properties
        {:floating true}}

       ;; Add titlebars to normal clients and dialogs
       {:rule_any {:type ["normal"
                          "dialog"]}
        :properties {:titlebars_enabled true}}

       {:rule {:role "browser"}
        :properties {:screen 1}
        :callback
        (fn [c]
          (if (not assigned-browser)
              (let [tag (awful.tag.find_by_name (awful.screen.focused) "web")]
                (tset c :above true)
                (tset c :floating true)
                (when tag
                  (awful.client.movetotag tag c))
                (set assigned-browser true))))}
       {:rule {:name "journal"}
        :properties {:screen 1
                     :tag "journal"
                     :above true
                     :placement awful.placement.centered
                     :floating true
                     :focus true}}
       {:rule {:name "notes"}
        :properties {:screen 1
                     :tag "notes"
                     :above true
                     :placement awful.placement.centered
                     :floating true
                     :focus true
                     }}])
