(local wibox (require "wibox"))
(local gears (require "gears"))
(local spawn (require "awful.spawn"))

(local UPDATE_SCRIPT "emacsclient -e \"(ruborcalor/org-pomodoro-time)\"")

(local org-pomo-widget [])

(set org-pomo-widget.widget
     (wibox.widget
      {:layout wibox.layout.fixed.horizontal
       :set_label
       (fn [self new-value]
         (set self.txt.markup
              (.. "<span size=\"large\" font_weight=\"bold\" color=\"#efaefb\">"
                  new-value "</span>")))
       1 {:align "center"
          :widget wibox.widget.textbox}
       2 {:id "txt"
          :widget wibox.widget.textbox}}))

(fn _G.update_org_pomo_widget [str]
  (when str
    (: org-pomo-widget.widget :set_label str)))

(fn worker []
  (gears.timer
   {:timeout 20
    :call_now true
    :autostart true
    :callback
    (fn []
      (spawn.easy_async UPDATE_SCRIPT _G.update_org_pomo_widget))})

  org-pomo-widget.widget)

(setmetatable org-pomo-widget {:__call (fn [_ ...] (worker ...))})
