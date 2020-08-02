(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))

(local dashboard (require :dashboard))

(local scratchpad (require :scratchpad))
(local w (require :workspaces))

(local tablex (require :pl.tablex))
(local lain (require :lain))

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
    (awful.spawn cmd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local centerwork_layout?
       (fn []
         (-> (awful.screen.focused)
             (. :selected_tag)
             (. :layout)
             (. :name)
             (= "centerwork"))))


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
        (key [:mod] "a" (scratchpad.toggle w.awesome-tag))
        (key [:mod] "u" (scratchpad.toggle w.journal-tag))
        (key [:mod] "y" (scratchpad.toggle w.yodo-tag))
        (key [:mod] "r" (scratchpad.toggle w.notes-tag))
        (key [:mod] "t" (scratchpad.toggle w.web-tag))
        (key [:mod] "0" (scratchpad.toggle w.dotfiles-tag))

        (key [:mod] "d" (fn []
                          (dashboard.dashboard_show)))

        ;; cycle clients
        ;;
        ;; (key [:mod] "Tab"
        ;;      (fn []
        ;;        (lain.util.menu_clients_current_tags {:width 350 } {:keygrabber true})))

        (key [:mod] "Tab"
             (fn []
               ;; move focus forward
               (awful.client.focus.byidx 1)

               ;; raise the focused client
               (when _G.client.focus
                 (_G.client.focus:raise))

               ;; swap to centered if relevant
               (when (centerwork_layout?)
                 (when _G.client.focus
                   (_G.client.focus:swap (awful.client.getmaster))))))

        (key [:mod :shift] "Tab"
             (fn []
               (awful.client.focus.byidx -1)
               (when _G.client.focus
                 (_G.client.focus:raise))

               (when (centerwork_layout?)
                 (when _G.client.focus
                   (_G.client.focus:swap (awful.client.getmaster))))))

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
        (key [:mod :shift] "n"
             (spawn-fn "ralphie awesome-create-tag"))


        ;; terminal
        (key [:mod] "Return"
             (fn []
               (let [current-tag (. (awful.screen.focused) :selected_tag)
                     name current-tag.name
                     str (.. "ralphie open-term " name)]
                 (awful.spawn str))))

        ;; emacs
        (key [:mod :shift] "Return"
             (fn []
               (let [current-tag (. (awful.screen.focused) :selected_tag)
                     name current-tag.name]
                 (awful.spawn (.. "ralphie open-emacs " name)))))

        ;; browser
        (key [:mod :shift] "b" (spawn-fn "google-chrome-stable"))

        ;; launcher (rofi)
        (key [:mod] "space" (spawn-fn "/usr/bin/rofi -show drun -modi drun"))

        ;; finder (thunar)
        (key [:mod] "e" (spawn-fn "/usr/bin/thunar"))

        ;; screenshots
        (key [:mod :shift] "s" (spawn-fn "/usr/bin/i3-scrot"))
        (key [:mod :shift] "a" (spawn-fn "/home/russ/.local/bin/screenshot-region"))

        ;; media controls
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

(global
 set_global_keys
 (fn []
   (_G.root.keys (gears.table.join global-keys tag-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exported to add to global rules
(set exp.clientkeys
     (gears.table.join
      ;; kill current client
      (key [:mod] "q" (fn [c] (c:kill)))

      ;; toggle floating
      (key [:mod] "f" awful.client.floating.toggle)

      ;; center on screen
      (key [:mod] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 ((+ awful.placement.scale
                     awful.placement.centered)
                  {:to_percent 0.75}))))

      ;; widen/shink windows
      (key [:mod :shift] "l"
           (fn [c]
             (if c.floating
                 (awful.placement.scale
                  c {:direction "right"
                     :by_percent 1.1})
                 (awful.tag.incmwfact 0.05))))
      (key [:mod :shift] "h"
           (fn [c]
             (if c.floating
                 (awful.placement.scale
                  c {:direction "right"
                     :by_percent 0.9})
                 (awful.tag.incmwfact -0.05))))
      (key [:mod :shift] "j"
           (fn [c]
             (if c.floating
                 (awful.placement.scale
                  c {:direction "down"
                     :by_percent 1.1})
                 (awful.client.incwfact 0.05))))
      (key [:mod :shift] "k"
           (fn [c]
             (if c.floating
                 (awful.placement.scale
                  c {:direction "down"
                     :by_percent 0.9})
                 (awful.client.incwfact -0.05))))


      ;; large centered
      (key [:mod :shift] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 ((+ awful.placement.scale
                     awful.placement.centered)
                  {:to_percent 0.9}))))

      ;; center without resizing
      (key [:mod :ctrl] "c"
           (fn [c]
             (-> c
                 (tset :floating true)
                 awful.placement.centered)))

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

;; TODO leave the buttons defs, but consume them in the global init
(global
 init_root_buttons
 (fn []
   (_G.root.buttons (gears.table.join
                     ;; (btn [] 1 mymainmenu:hide)
                     ;; (btn [] 3 mymainmenu:toggle)
                     (btn [] 4 awful.tag.viewnext)
                     (btn [] 5 awful.tag.viewprev)))))

exp
