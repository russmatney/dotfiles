(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(local ralphie (require "ralphie"))
(require "awful.autofocus")
(local naughty (require "naughty"))

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

(local journal-workspace
       {:tag-name "journal"
        :emacs-file "~/todo/journal.org"})

(local notes-workspace
       {:tag-name "notes"
        :emacs-file "~/Dropbox/notes/readme.org"})

(local yodo-workspace
       {:tag-name "yodo"
        :browser-url "http://localhost:4200"
        ;; "http://localhost:4222/devcards.html"
        })

(local web-workspace
       {:tag-name "web"
        :browser-url "chrome://newtab"})

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

(fn create-or-toggle-scratchpad
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

        ;; journal scratchpad
        (key [:mod] "u" (create-or-toggle-scratchpad journal-workspace))
        ;; roam scratchpad
        (key [:mod] "r" (create-or-toggle-scratchpad notes-workspace))
        ;; yodo scratchpad
        (key [:mod] "y" (create-or-toggle-scratchpad yodo-workspace))
        ;; web scratchpad
        (key [:mod] "t" (create-or-toggle-scratchpad web-workspace))

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

(local modkey "Mod4")

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
