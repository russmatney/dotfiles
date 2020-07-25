(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(local ralphie (require "ralphie"))
(require "awful.autofocus")
(local naughty (require "naughty"))
(require-macros "macros")

(local hotkeys_popup (require "awful.hotkeys_popup"))

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
        (key [:mod] "u" awful.client.urgent.jumpto)

        ;; cycle clients
        (key [:mod] "Tab" (fn []
                            (awful.client.focus.history.previous)
                            (when client.focus
                              (client.focus:raise))))

        ;; terminal
        (key [:mod] "Return" (fn [] (awful.spawn terminal)))

        ;; emacs
        (key [:mod :shift] "Return" (fn [] (awful.spawn "ralphie open-emacs")))

        ;; browser
        (key [:mod :shift] "b" (fn [] (awful.spawn "exo-open --launch WebBrowser")))

        ;; launcher (rofi)
        (key [:mod] "space" (fn [] (awful.spawn "/usr/bin/rofi -show drun -modi drun")))

        ;; finder (thunar)
        (key [:mod] "e" (fn [] (awful.spawn "/usr/bin/thunar")))

        ;; widen/shink window
        (key [:mod :shift] "l" (fn [] (awful.tag.incmwfact 0.05)))
        (key [:mod :shift] "h" (fn [] (awful.tag.incmwfact -0.05)))

        ;; screenshots
        (key [:mod :shift] "s" (fn [] (awful.spawn "/usr/bin/i3-scrot")))
        (key [:mod :shift] "a" (fn [] (awful.spawn "/home/russ/.local/bin/screenshot-region")))

        (key [] "XF86AudioPlay" (fn [] (awful.spawn "spotifycli --playpause")))

        ;; minimized
        (key [:mod :shift] "n" (fn [] (let [c (awful.client.restore)]
                                        (when c (tset client :focus c) (c:raise)))))
        ))

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
                   (if (= keyed-tag.name current-tag.name)
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
