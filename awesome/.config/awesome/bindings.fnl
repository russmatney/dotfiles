(local gears (require "gears"))
(local awful (require "awful"))

(local fun (require "fun"))
(local tablex (require :pl.tablex))

;; (local dashboard (require :dashboard.dashboard))
(local helpers (require :dashboard.helpers))
(local restart-helper (require "./restart"))

(local exp {})

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

(fn key
  [mods key-code fun opt]
  (let [opt (or opt {})]
    (awful.key (map-mods mods) key-code fun opt)))

(set exp.key key)

(fn btn
  [mods btn-code fun]
  (awful.button (map-mods mods) btn-code fun))

(set exp.btn btn)

(fn spawn-fn
  [cmd]
  (fn []
    (awful.spawn cmd false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn centerwork_layout? []
  (-> (awful.screen.focused)
      (. :selected_tag)
      (. :layout)
      (. :name)
      ((fn [n]
         (or
          (= n "centerworkh")
          (= n "centerwork"))))))

(local global-keys
       (gears.table.join
        ;; helpers
        (key [:mod :shift] "r" restart-helper.save_state_and_restart)

        ;; walk tags
        (key [:mod] "Left" awful.tag.viewprev)
        (key [:mod] "Right" awful.tag.viewnext)

        ;; previous tag
        (key [:mod] "Escape" awful.tag.history.restore)

        ;; TODO move all these bindings into ralphie itself
        ;; ralphie rofi
        (key [:mod] "x" (spawn-fn "ralphie rofi"))

        ;; scratchpads
        ;; TODO should pull the letter from workspaces.org
        ;; and write into ralphie-build-and-install
        (key [:mod] "u" (spawn-fn "ralphie-toggle-scratchpad journal"))
        (key [:mod] "y" (spawn-fn "ralphie-toggle-scratchpad yodo-app"))
        (key [:mod] "r" (spawn-fn "ralphie-toggle-scratchpad notes"))
        (key [:mod] "t" (spawn-fn "ralphie-toggle-scratchpad web"))
        (key [:mod] "a" (spawn-fn "ralphie-toggle-scratchpad slack"))
        (key [:mod] "s" (spawn-fn "ralphie-toggle-scratchpad spotify"))

        ;; TODO rename to 'open-workspace'
        (key [:mod] "d" (spawn-fn "ralphie-clean-up-workspaces"))
        (key [:mod] "o" (spawn-fn "ralphie-create-workspace"))

        ;; cycle layouts
        (key [:mod] "Tab"
             (fn []
               (let [scr (awful.screen.focused)]
                 (awful.layout.inc 1 scr _G.layouts))))
        (key [:mod :shift] "Tab"
             (fn []
               (let [scr (awful.screen.focused)]
                 (awful.layout.inc -1 scr _G.layouts))))

        ;; cycle workspaces
        (key [:mod] "n"
             (fn []
               (let [scr (awful.screen.focused)
                     current-tag scr.selected_tag
                     idx (if current-tag current-tag.index 1)
                     tag-count (tablex.size scr.tags)
                     next-idx (- idx 1)
                     next-idx (if (< next-idx 1)
                                  tag-count
                                  next-idx)
                     next-tag (. scr.tags next-idx)]
                 (next-tag:view_only))))
        (key [:mod] "p"
             (fn []
               (let [scr (awful.screen.focused)
                     current-tag scr.selected_tag
                     idx (if current-tag current-tag.index 1)
                     tag-count (tablex.size scr.tags)
                     next-idx (+ idx 1)
                     next-idx (if (> next-idx tag-count)
                                  1
                                  next-idx)
                     next-tag (. scr.tags next-idx)]
                 (next-tag:view_only))))

        (key [:mod :shift] "n" (spawn-fn "ralphie-swap-workspace-index down"))
        (key [:mod :shift] "p" (spawn-fn "ralphie-swap-workspace-index up"))

        ;; terminal
        (key [:mod] "Return"
             (fn []
               (let [current-tag (. (awful.screen.focused) :selected_tag)
                     name current-tag.name
                     str (.. "ralphie-open-term " name)]
                 (awful.spawn str))))

        ;; emacs
        (key [:mod :shift] "Return" (spawn-fn "ralphie open-emacs"))

        ;; browser
        ;; (key [:mod :shift] "b" (spawn-fn "google-chrome-stable"))
        (key [:mod :shift] "b" (spawn-fn "firefox"))

        ;; launcher (rofi)
        (key [:mod] "space" (spawn-fn "/usr/bin/rofi -show drun -modi drun"))

        ;; finder (thunar)
        (key [:mod] "e" (spawn-fn "/usr/bin/thunar"))

        ;; screenshots
        (key [:mod :shift] "s" (spawn-fn "ralphie screenshot full"))
        (key [:mod :shift] "a" (spawn-fn "ralphie screenshot region"))

        ;; brightness
        (key [] "XF86MonBrightnessUp" (spawn-fn "light -A 5"))
        (key [] "XF86MonBrightnessDown" (spawn-fn "light -U 5"))

        ;; media controls
        ;; TODO play-pause should create spotify if its not open
        (key [] "XF86AudioPlay" (spawn-fn "spotifycli --playpause"))
        (key [] "XF86AudioNext" (spawn-fn "playerctl next"))
        (key [] "XF86AudioPrev" (spawn-fn "playerctl previous"))
        (key [] "XF86AudioMute" (spawn-fn "pactl set-sink-mute @DEFAULT_SINK@ toggle"))
        (key [] "XF86AudioRaiseVolume" (spawn-fn "pactl set-sink-volume @DEFAULT_SINK@ +5%"))
        (key [] "XF86AudioLowerVolume" (spawn-fn "pactl set-sink-volume @DEFAULT_SINK@ -5%"))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbered Tag global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var tag-keys [])

(for [it 0 10]
  (set tag-keys
       (gears.table.join
        tag-keys

        ;; show tag (workspace)
        (key [:mod] (.. "#" (+ 9 it))
             (fn []
               (let [scr (awful.screen.focused)
                     keyed-tag (. scr.tags it)
                     current-tag scr.selected_tag]
                 (if keyed-tag
                     (helpers.tag_back_and_forth keyed-tag.index)
                     (let []
                       ;; create tag
                       ;; TODO fetch name  from config for index
                       ;; include other tag config?
                       (awful.tag.add (.. "num" it) {:layout (. layouts 1)
                                                     :selected true}))))))

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

(global
 set_global_keys
 (fn []
   (_G.root.keys (gears.table.join global-keys tag-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn is-emacs [c] (= c.class "Emacs"))

;; too slow
(fn emacs-move [dir]
  ;; (print "attempting to move")
  ;; (print dir)
  (let [cmd
        (.. "emacsclient -e '(evil-window-" dir " 1)'")]
    ;; (print cmd)
    (awful.spawn cmd)))

(fn focus-move [dir centerwork-dir centerwork-dir2]
  ;; TODO should move floating windows if floating
  ;; TODO consider toggling when there is a floating window
  (if (centerwork_layout?)
      (do
        (awful.client.focus.bydirection centerwork-dir)
        (awful.client.focus.bydirection centerwork-dir2)
        (when _G.client.focus
          (_G.client.focus:swap (awful.client.getmaster))))

      (awful.client.focus.bydirection dir)))

(fn move-client [c dir]
  (if
   (= "right" dir) (set c.x (+ c.x 10))
   (=  "left" dir) (set c.x (+ c.x -10))
   ;; y 0 at top
   (= "up" dir) (set c.y (+ c.y -10))
   (= "down" dir) (set c.y (+ c.y 10)))
  )

;; exported to add to global rules
(set exp.clientkeys
     (gears.table.join
      ;; kill current client
      (key [:mod] "q" (fn [c] (c:kill)))

      ;; toggle floating
      (key [:mod] "f" awful.client.floating.toggle)

      ;; focus movement
      (key [:mod :shift] "l" (fn [c]
                               (if c.floating
                                   (move-client c "right")
                                   (focus-move "right" "right" "up"))))
      (key [:mod :shift] "h" (fn [c]
                               (if c.floating
                                   (move-client c "left")
                                   (focus-move "left" "left" "down"))))
      (key [:mod :shift] "j" (fn [c]
                               (if c.floating
                                   (move-client c "down")
                                   (focus-move "down" "right" "down"))))
      (key [:mod :shift] "k" (fn [c]
                               (if c.floating
                                   (move-client c "up")
                                   (focus-move "up" "left" "up"))))

      ;; widen/shink windows
      (key [:ctrl :shift] "l"
           (fn [c]
             (if c.floating
                 (do
                   (awful.placement.scale
                    c {:direction "right"
                       :by_percent 1.1})
                   (awful.placement.scale
                    c {:direction "left"
                       :by_percent 1.1})
                   )
                 (awful.tag.incmwfact 0.05))))
      (key [:ctrl :shift] "h"
           (fn [c]
             (if c.floating
                 (do
                   (awful.placement.scale
                    c {:direction "left"
                       :by_percent 0.9})
                   (awful.placement.scale
                    c {:direction "right"
                       :by_percent 0.9})
                   )
                 (awful.tag.incmwfact -0.05))))
      (key [:ctrl :shift] "j"
           (fn [c]
             (if c.floating
                 (do
                   (awful.placement.scale
                    c {:direction "down"
                       :by_percent 1.1})
                   (awful.placement.scale
                    c {:direction "up"
                       :by_percent 1.1})
                   )
                 (awful.client.incwfact 0.05))))
      (key [:ctrl :shift] "k"
           (fn [c]
             (if c.floating
                 (do
                   (awful.placement.scale
                    c {:direction "up"
                       :by_percent 0.9})
                   (awful.placement.scale
                    c {:direction "down"
                       :by_percent 0.9})
                   )
                 (awful.client.incwfact -0.05))))

      ;; center on screen
      (key [:mod] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 ((+ awful.placement.scale
                     awful.placement.centered)
                  {:honor_padding true
                   :honor_workarea true
                   :to_percent 0.75}))))

      ;; large centered
      (key [:mod :shift] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 ((+ awful.placement.scale
                     awful.placement.centered)
                  {:honor_padding true
                   :honor_workarea true
                   :to_percent 0.9}))))

      ;; center without resizing
      (key [:mod :ctrl] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 (awful.placement.centered
                  {:honor_padding true
                   :honor_workarea true}))))

      ;; swap with master
      (key [:mod :ctrl] "Return" (fn [c] (c:swap (awful.client.getmaster))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exported to add to global rules
(set exp.clientbuttons
     (gears.table.join
      (btn [] 1 (fn [c]
                  (tset _G.client :focus c)
                  (c:raise)))
      (btn [:mod] 1 awful.mouse.client.move)
      (btn [:mod] 3 awful.mouse.client.resize)))

(global
 init_root_buttons
 (fn []
   (_G.root.buttons (gears.table.join
                     ;; (btn [] 1 mymainmenu:hide)
                     ;; (btn [] 3 mymainmenu:toggle)
                     (btn [] 4 awful.tag.viewnext)
                     (btn [] 5 awful.tag.viewprev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titlebar buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set exp.titlebarbuttons
     (fn [c]
       (gears.table.join
        (btn [] 1 (fn []
                    (set client.focus c)
                    (c:raise)
                    (awful.mouse.client.move c)))
        (btn [] 3 (fn []
                    (set client.focus c)
                    (c:raise)
                    (awful.mouse.client.resize c))))))

exp
