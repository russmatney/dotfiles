(local awful (require "awful"))
(local beautiful (require "beautiful"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage Signal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_manage_signal
 (fn []
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
               (awful.rules.apply c)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus/Unfocus Signal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_focus_signals
 (fn []
   (_G.client.connect_signal
    "focus"
    (fn [c] (set c.border_color beautiful.border_focus)))

   (_G.client.connect_signal
    "unfocus"
    (fn [c] (set c.border_color beautiful.border_normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrange Signal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn no-borders? [s c]
  (let [layout (awful.layout.getname (awful.layout.get s))]
    (or
     (= (. c :name) "Yodo Electron")
     (= (. c :name) "yodo/bar")
     c.maximized
     (= layout "max")
     (= layout "fullscreen")
     (let [tiled (awful.client.tiled c.screen)]
       (= #tiled 1)))))

(global
 init_arrange_signal
 (fn []
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
            (if (no-borders? s c)
                ;; NOTE: also handled in focus, but that does not cover maximizing from a
                ;; tiled state (when the client had focus).
                (set c.border_width 0)

                (or c.floating (= layout "floating"))
                (set c.border_width beautiful.border_width)

                (tset c :border_width beautiful.border_width)))))))))
