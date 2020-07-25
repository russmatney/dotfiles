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

(fn spawn-fn
  [cmd]
  (fn []
    (awful.spawn cmd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create or toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn create-emacs-client
  [frame-name]
  (awful.spawn.with_shell
   (.. "emacsclient --alternate-editor='' --no-wait --create-frame -F '(quote (name . \""
       frame-name
       "\"))' --display $DISPLAY")))

(fn create-or-toggle-scratchpad
  [tag-and-client-name]
  (fn []
    (print "called create or toggle scratchpad")
    (let [s (awful.screen.focused)
          journal-tag (awful.tag.find_by_name s tag-and-client-name)
          journal-tag-clients (when journal-tag (journal-tag:clients))
          any-tag-clients? (when journal-tag (> (length journal-tag-clients) 0))]
      (if
       ;; if tag and a client, toggle tag
       (and journal-tag any-tag-clients?)
       (do
         (print "found journal tag and clients in that tag - toggling tag")
         (awful.tag.viewtoggle journal-tag))

       ;; if tag but no client, create client
       (and journal-tag (not any-tag-clients?))
       (do
         (print "found journal tag and NO clients - creating client")
         (print journal-tag-clients)
         (print (length journal-tag-clients))
         (each [k v (ipairs journal-tag-clients)]
           (print k v))
         (create-emacs-client tag-and-client-name))

       ;; no tag? create it
       (not journal-tag)
       (do
         (print "no journal tag, creating tag and client")
         (awful.tag.add tag-and-client-name
                        {:screen s
                         :layout awful.layout.suit.floating})
         ;; TODO should only create here if no journal client exists
         (create-emacs-client tag-and-client-name))
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
        (key [:mod] "u" (create-or-toggle-scratchpad "journal"))

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

        ;; screenshots
        (key [:mod :shift] "s" (spawn-fn "/usr/bin/i3-scrot"))
        (key [:mod :shift] "a" (spawn-fn "/home/russ/.local/bin/screenshot-region"))

        (key [] "XF86AudioPlay" (spawn-fn "spotifycli --playpause"))

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
