(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(local ralphie (require "ralphie"))
(require "awful.autofocus")
(local naughty (require "naughty"))
(require-macros "macros")

(local hotkeys_popup (require "awful.hotkeys_popup"))
(local beautiful (require "beautiful"))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create or toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local journal-file "~/todo/journal.org")
(local notes-file "~/Dropbox/notes/readme.org")

;; TODO refactor into tags/workspaces/clients datastructure (frame-name, filename, tag name)
(fn create-emacs-client
  [frame-name filename]
  (awful.spawn.with_shell
   (.. "emacsclient --alternate-editor='' --no-wait --create-frame "
       filename
       " -F '(quote (name . \""
       frame-name
       "\"))' --display $DISPLAY")))

(fn create-or-toggle-scratchpad
  [tag-and-client-name x-file]
  (fn []
    (let [s (awful.screen.focused)
          x-tag (awful.tag.find_by_name s tag-and-client-name)
          x-tag-clients (when x-tag (x-tag:clients))
          any-tag-clients? (when x-tag (> (length x-tag-clients) 0))
          x-client (when any-tag-clients? (. x-tag-clients 1))]
      (if
       ;; if tag and a client, toggle tag, focus client
       (and x-tag x-client)
       (do
         (awful.tag.viewtoggle x-tag)
         (when (and x-client (not x-client.active))
           (tset client :focus x-client)))

       ;; if tag but no client, create client
       (and x-tag (not x-client))
       (create-emacs-client tag-and-client-name x-file)

       ;; no tag? create it
       (not x-tag)
       (do
         (awful.tag.add tag-and-client-name
                        {:screen s
                         :layout awful.layout.suit.floating})
         ;; TODO should only create here if no x client exists
         (create-emacs-client tag-and-client-name x-file))
       ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local terminal (or (os.getenv "TERMINAL") "lxterminal"))

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

        ;; urgent tag
        (key [:mod] "u" (create-or-toggle-scratchpad "journal" journal-file))
        ;; urgent tag
        (key [:mod] "r" (create-or-toggle-scratchpad "notes" notes-file))

        ;; cycle clients
        (key [:mod] "Tab" (fn []
                            (awful.client.focus.history.previous)
                            (when client.focus
                              (client.focus:raise))))

        ;; terminal
        (key [:mod] "Return" (spawn-fn terminal))

        ;; emacs
        (key [:mod :shift] "Return" (spawn-fn "ralphie open-emacs"))

        ;; browser
        (key [:mod :shift] "b" (spawn-fn "exo-open --launch WebBrowser"))

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
               (let [screen (awful.screen.focused)
                     keyed-tag (. screen.tags it)
                     current-tag screen.selected_tag]
                 (when keyed-tag
                   (if (and current-tag (= keyed-tag.name current-tag.name))
                       (awful.tag.history.restore screen 1)
                       (keyed-tag:view_only))))))

        ;; add tag to current perspective
        (key [:mod :ctrl] (.. "#" (+ 9 it))
             (fn []
               (let [screen (awful.screen.focused)
                     scr-tag (. screen.tags it)]
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
        (key [:mod] "t" (fn [c] (tset c :ontop (not c.ontop))))

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

(local modkey "Mod4")

(local clientbuttons
       (gears.table.join
        (btn [] 1 (fn [c]
                    (tset client :focus c)
                    (c:raise)))
        (btn [:mod] 1 awful.mouse.client.move)
        (btn [:mod] 3 awful.mouse.client.resize)))

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

       {:rule {:name "journal"}
        :properties {:screen 1
                     :tag "journal"
                     :above true
                     :placement awful.placement.centered
                     :floating true
                     :focus true
                     :width (* (. (. (awful.screen.focused) :geometry) :width) 0.7)
                     :height (* (. (. (awful.screen.focused) :geometry) :height) 0.7)}}
       {:rule {:name "notes"}
        :properties {:screen 1
                     :tag "notes"
                     :above true
                     :placement awful.placement.centered
                     :floating true
                     :focus true
                     :width (* (. (. (awful.screen.focused) :geometry) :width) 0.7)
                     :height (* (. (. (awful.screen.focused) :geometry) :height) 0.7)
                     }}])
