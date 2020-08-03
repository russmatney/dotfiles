(local wibox (require "wibox"))
(local gears (require "gears"))
(local awful (require "awful"))

(local mytaglist (require "mytaglist"))
(local focus_widget (require "widgets.focus"))

(local todo_widget (require "awesome-wm-widgets.todo-widget.todo"))
;; (local stackoverflow_widget (require "awesome-wm-widgets.stackoverflow-widget.stackoverflow"))
(local pomodoro_widget (require "awesome-wm-widgets.pomodoroarc-widget.pomodoroarc"))
(local ram_widget (require "awesome-wm-widgets.ram-widget.ram-widget"))
;; (local batteryarc_widget (require"awesome-wm-widgets.batteryarc-widget.batteryarc"))
;; (local volumebar_widget (require"awesome-wm-widgets.volumebar-widget.volumebar"))
(local brightness_widget (require "awesome-wm-widgets.brightness-widget.brightness"))
;; (local weather_widget (require "awesome-wm-widgets.weather-widget.weather"))
(local spotify_widget (require"awesome-wm-widgets.spotify-widget.spotify"))

(local awful (require "awful"))

(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIBAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a textclock widget
(local mytextclock (wibox.widget.textclock "%H:%M "))

(local blue "#9EBABA")
(local separator (wibox.widget.textbox
                  (.. "         <span color=\""
                      blue
                      "\">| </span>         ")))

;; Create a wibox for each screen and add it
(local taglist_buttons
       (gears.table.join
        (bindings.btn [] 1 (fn [t] (t:view_only)))
        (bindings.btn [:mod] 1
                      (fn [t]
                        (if _G.client.focus
                            (_G.client.focus:move_to_tag t))))
        (bindings.btn [] 3 awful.tag.viewtoggle)
        (bindings.btn [:mod] 3
                      (fn [t]
                        (if _G.client.focus
                            (_G.client.focus:toggle_tag t))))
        (bindings.btn [] 4 (fn [t] (awful.tag.viewnext t.screen)))
        (bindings.btn [] 5 (fn [t] (awful.tag.viewprev t.screen)))))

(local tasklist_buttons
       (gears.table.join
        (bindings.btn
         []  1
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
        (bindings.btn [] 4 (fn [] (awful.client.focus.byidx 1)))
        (bindings.btn [] 5 (fn [] (awful.client.focus.byidx -1)))))


(local color-wheel
       [
        "#D1908E"
        "#EBCAA2"
        "#F99482"
        "#F27BE9"
        "#B9E3B7"
        "#FBC2CC"
        "#6DE9F0"
        "#D1908E"
        "#EBCAA2"
        "#F99482"
        "#F27BE9"
        ])

(global
 create_taglist
 (fn [s]
   (mytaglist
    {:screen s
     :filter awful.widget.taglist.filter.all

     :buttons taglist_buttons
     :update_function awful.widget.common.list_update

     :style
     {:bg_focus "#617082"
      :shape gears.shape.powerline}

     :layout
     {:spacing 0
      ;; :spacing_widget {:color  "#dddddd"
      ;;                  :shape  gears.shape.powerline
      ;;                  :widget wibox.widget.separator}
      :layout  wibox.layout.fixed.horizontal}

     :widget_template
     {1 {1 {1 {1 {1 {:id     "index_role"
                     :widget wibox.widget.textbox
                     :opacity 0.9}
                  :margins 4
                  :widget  wibox.container.margin}
               ;; :bg     "#1D334C#617082"
               :id "circle_role"
               :shape  gears.shape.circle
               :widget wibox.container.background}
            2 {1 {:id     "icon_role"
                  :widget wibox.widget.imagebox}
               :margins 2
               :widget  wibox.container.margin}
            3 {:id     "text_role"
               :widget wibox.widget.textbox}
            :layout wibox.layout.fixed.horizontal}
         :left  18
         :right 18
         :widget wibox.container.margin}
      :id     "background_role"
      :widget wibox.container.background

      ;; Adds support for hover colors and an index label
      :create_callback
      (fn [self c3 index objects]
        (-> (self:get_children_by_id "index_role")
            (. 1)
            (tset :markup (..  "<b> " index " </b>")))

        (-> (self:get_children_by_id "circle_role")
            (. 1)
            (tset :bg (. color-wheel index)))

        (self:connect_signal
         "mouse::enter" (fn [] (tset self :bg "#ff0000")))

        (self:connect_signal
         "mouse::leave" (fn [] (tset self :bg nil))))

      :update_callback
      (fn [self c3 index objects]
        (-> (self:get_children_by_id "index_role")
            (. 1)
            (tset :markup (.. "<b> " index " </b>")))
        )}})))

;; TODO give global names a larger font size, or green flycheck underline
(global
 init_screen
 (fn []
   (awful.screen.connect_for_each_screen
    (fn [s]
      ;; Create an imagebox widget which will contains an icon indicating which layout we're using.
      ;; We need one layoutbox per screen.
      (set s.mylayoutbox (awful.widget.layoutbox s))

      ;; layoutbox cycling
      ;; TODO move to bindings file
      (s.mylayoutbox:buttons
       (gears.table.join
        (bindings.btn [] 1 (fn [] (awful.layout.inc 1)))
        (bindings.btn [] 3 (fn [] (awful.layout.inc -1)))
        (bindings.btn [] 4 (fn [] (awful.layout.inc 1)))
        (bindings.btn [] 5 (fn [] (awful.layout.inc -1)))))

      ;; Create a taglist widget
      (set s.mytaglist (create_taglist s))

      ;; Create the wibox
      (set s.mywibox (awful.wibar {:position "top" :screen s}))

      ;; Add widgets to the wibox
      (s.mywibox:setup
       {:layout wibox.layout.align.horizontal
        1  {:layout wibox.layout.fixed.horizontal ;; Left widgets
            1 s.mylayoutbox
            2 separator
            3 s.mytaglist
            4 separator
            5 (focus_widget)}

        ;; Middle widget
        2 {:layout wibox.layout.fixed.horizontal
           :expand "none"
           1 pomodoro_widget
           2 (ram_widget)
           3 (todo_widget)
           4 (brightness_widget)
           5 (spotify_widget)
           ;; 4 (batteryarc_widget) ;; not necessary on algo
           ;; 5 (stackoverflow_widget
           ;;    {:limit 10
           ;;     :tagged "clojure,fennel,babashka"})
           ;; 6 (volumebar_widget
           ;;    {:main_color "#af13f7"
           ;;     :mute_color "#ff0000"
           ;;     :inc_volume_cmd "pactl set-sink-volume @DEFAULT_SINK@ +5%"
           ;;     :dec_volume_cmd "pactl set-sink-volume @DEFAULT_SINK@ -5%"
           ;;     :get_volume_cmd "get-volume"
           ;;     :tog_volume_cmd "pactl set-sink-mute @DEFAULT_SINK@ toggle"
           ;;     :width 80
           ;;     :shape "rounded_bar"
           ;;     :margins 4})
           ;; 8 (weather_widget
           ;;    {:api_key "$OPENWEATHERMAP_APIKEY"
           ;;     :coordinates [40.6782 -73.9442]
           ;;     :time_format_12h   true
           ;;     :units   "imperial"
           ;;     :both_units_widget   true
           ;;     :font_name   "Carter One"
           ;;     :icons   "VitalyGorbachev"
           ;;     :show_hourly_forecast   true
           ;;     :show_daily_forecast   true
           ;;     :icons_extension ".svg"}
           ;;    )
           }

        ;; Right widgets
        3 {:layout wibox.layout.fixed.horizontal
           1 separator
           2 (wibox.widget.systray)
           3 mytextclock}})))))
