(local wibox (require "wibox"))
(local gears (require "gears"))

(local helpers (require "dashboard.helpers"))

(local dirty-repos-widget (require "widgets.dirty-repos"))
(local org-pomo-widget (require "widgets.org-pomodoro"))

;; (local pomodoro-widget (require "awesome-wm-widgets.pomodoroarc-widget.pomodoroarc"))
(local batteryarc-widget (require"awesome-wm-widgets.batteryarc-widget.batteryarc"))
(local spotify-widget (require"awesome-wm-widgets.spotify-widget.spotify"))

(local awful (require "awful"))

(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIBAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a textclock widget
(local mytextclock (wibox.widget.textclock "%H:%M "))

(local blue "#9EBABA")
(local separator (wibox.widget.textbox
                  (.. "<span color=\"" blue "\"> | </span>")))

;; Create a wibox for each screen and add it
(local taglist_buttons
       (gears.table.join
        (bindings.btn [] 1 (fn [t] (helpers.tag_back_and_forth t.index)))
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

(local color-wheel
       ["#D1908E"
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

(local dark-color-wheel
       ["#1b3a4C"
        "#1b448C"
        "#1e334b"
        "#5f3e3C"
        "#3b3a3C"
        "#1a3b4C"
        "#1b3a4C"
        "#1b448C"
        "#1b3a4C"
        "#be33aC"
        "#1b3a4C"
        "#1b448C"
        "#1e334b"
        "#5f3e3C"
        "#3b3a3C"
        "#1a3b4C"
        "#1b3a4C"
        "#1b448C"
        "#1b3a4C"
        "#be33aC"])

(local fg-occupied "#ddddddee")
(local bg-occupied "#1b448C44") ;; blue
(local fg-empty "#adadad99")
(local bg-empty "#1a3b4C") ;; blue/green

(local bg-hover "#d2834399") ;; orange, transparent
;; (local fg-hover "#d2834399") ;; orange, transparent

(local bg-focus "#d28343") ;; orange
(local fg-focus "white") ;; orange

(local bg-urgent "#3b3a3C") ;; brown
(local fg-urgent "#d28343dd") ;; orange

(global
 create_taglist
 (fn [s]
   (awful.widget.taglist
    {:screen s
     :filter awful.widget.taglist.filter.all

     :buttons taglist_buttons
     :update_function awful.widget.common.list_update

     :style
     {:bg_focus bg-focus
      :fg_focus fg-focus
      :fg_urgent fg-urgent
      :bg_urgent bg-urgent
      :fg_empty fg-empty
      :bg_empty bg-empty
      :fg_occupied fg-occupied
      :bg_occupied bg-occupied
      :shape gears.shape.powerline}

     :layout
     {:spacing 0
      ;; :spacing_widget {:color  "#dddddd"
      ;;                  :shape  gears.shape.powerline
      ;;                  :widget wibox.widget.separator}
      :layout  wibox.layout.fixed.horizontal}

     :widget_template
     {1 {1 {1 {1 {1 {:id     "index_role"
                     :widget wibox.widget.textbox}
                  :margins 6
                  :widget  wibox.container.margin}
               :id "circle_role"
               :shape  gears.shape.circle
               :widget wibox.container.background}
            2 {1 {:id     "icon_role"
                  :widget wibox.widget.imagebox}
               :margins -2
               :widget  wibox.container.margin}
            3 {:id   "text_role"
               :font "Roboto Mono Nerd Font 24"
               :widget wibox.widget.textbox}
            :layout wibox.layout.fixed.horizontal}
         :left  18
         :right 18
         :widget wibox.container.margin}
      :id     "background_role"
      :widget wibox.container.background

      ;; Adds support for hover colors and an index label
      :create_callback
      (fn [self _c3 index _objects]
        (-> (self:get_children_by_id "index_role")
            (. 1)
            (tset :markup (..  "<span color='"
                               (. color-wheel index)
                               "'><b> " index " </b></span>")))

        (-> (self:get_children_by_id "circle_role")
            (. 1)
            (tset :bg (. dark-color-wheel index)))

        (self:connect_signal
         "button::press"
         (fn [] (tset self :backup nil)))

        (self:connect_signal
         "mouse::enter"
         (fn []
           (if (not (= bg-hover self.bg))
               (do
                 (tset self :backup self.bg)
                 (tset self :bg bg-hover)))))

        (self:connect_signal
         "mouse::leave"
         (fn []
           (if self.backup
               (tset self :bg self.backup)
               ;; (tset self :bg nil)
               ))))

      :update_callback
      (fn [self _c3 index _objects]
        (-> (self:get_children_by_id "index_role")
            (. 1)
            (tset :markup (..  "<span color='"
                               (. color-wheel index)
                               "'><b> " index " </b></span>"))))}})))

(fn get-hostname []
  "ty to https://gist.github.com/h1k3r/089d43771bdf811eefe8 for this."
  (let [f (io.popen "/bin/hostname")
        hostname (or (f:read "*a") "")]
    (f:close)
    (string.gsub hostname "\n$" "")))

(fn is-vader []
  (= (get-hostname) "vader"))

(global
 init_screen
 (fn []
   (awful.screen.connect_for_each_screen
    (fn [s]
      ;; set padding for top-status bar
      (tset s :padding {
                        :top 0
                        ;; :top 100 ;; matches status bar height
                        :bottom s.padding.bottom
                        :left s.padding.left
                        :right s.padding.right})

      ;; Create an imagebox widget which will contains an icon indicating which layout we're using.
      ;; We need one layoutbox per screen.
      (set s.mylayoutbox
           (awful.widget.layoutbox s))

      ;; layoutbox cycling
      ;; TODO move to bindings file
      (s.mylayoutbox:buttons
       (gears.table.join
        (bindings.btn [] 1 (fn [] (awful.layout.inc 1)))
        (bindings.btn [] 3 (fn [] (awful.layout.inc -1)))
        (bindings.btn [] 4 (fn [] (awful.layout.inc 1)))
        (bindings.btn [] 5 (fn [] (awful.layout.inc -1)))))

      ;; Create a taglist widget
      (set s.mytaglist (_G.create_taglist s))

      ;; Create the wibox
      (set s.mywibox (awful.wibar {:position "bottom" :screen s}))

      ;; Add widgets to the wibox
      (s.mywibox:setup
       {:layout wibox.layout.align.horizontal
        1  {:layout wibox.layout.fixed.horizontal ;; Left widgets
            1 s.mytaglist}

        ;; Middle widget
        2 {:layout wibox.layout.fixed.horizontal
           :expand "none"
           ;; 1 (pomodoro-widget)
           1 (spotify-widget)
           2 (when (is-vader) (batteryarc-widget))
           3 separator
           4 (dirty-repos-widget)
           5 separator
           6 (org-pomo-widget)
           }

        ;; Right widgets
        3 {:layout wibox.layout.fixed.horizontal
           1 separator
           2 (wibox.widget.systray)
           3 mytextclock}})))))
