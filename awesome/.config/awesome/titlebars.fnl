(local awful (require "awful"))
(local wibox (require "wibox"))

(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titlebars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_request_titlebars
 (fn []
   ;; add signal for toggling title bars on float toggle
   ;; (client.connect_signal
   ;;  "property::floating"
   ;;  (fn [c]
   ;;    ;; https://www.reddit.com/r/awesomewm/comments/bki1md/show_titlebar_only_when_window_is_floating/
   ;;    (if c.floating
   ;;        (awful.titlebar.show c)
   ;;        (awful.titlebar.hide c))))

   ;; Add a titlebar if titlebars_enabled is set to true in the rules.
   (_G.client.connect_signal
    "request::titlebars"
    (fn [c]
      (let [buttons (bindings.titlebarbuttons c)
            titlebar (awful.titlebar c {:size 30})]

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
             4 (awful.titlebar.widget.closebutton    c)
             :layout (wibox.layout.fixed.horizontal)
             }
          :layout wibox.layout.align.horizontal}))))))
