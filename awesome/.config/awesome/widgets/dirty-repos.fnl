(local wibox (require "wibox"))
(local spawn (require "awful.spawn"))
(local tablex (require "pl.tablex"))
(local awful (require "awful"))
(local gears (require "gears"))
(local beautiful (require "beautiful"))

(local icons (require "icons"))

(local UPDATE_REPOS "bash -c \"ralphie update-dirty-repos\"")

(fn row [item]
  (wibox.widget
   {:bg beautiful.bg_normal
    :widget wibox.container.background
    1 {:layout wibox.container.margin
       :margins 8
       1 {:align "left"
          :text item.name
          :widget wibox.widget.textbox}}}))

(local popup
       (awful.popup {:bg beautiful.bg_normal
                     :border_color beautiful.bg_focus
                     :border_width 1
                     :maximum_width 400
                     :offset {:y 5}
                     :ontop true
                     :shape gears.shape.rounded_rect
                     :visible false
                     :widget []}))

(local repos-widget [])

(set repos-widget.widget
     (wibox.widget
      {:layout wibox.layout.fixed.horizontal
       :set_count
       (fn [self new-value]
         (local str (.. "<span size=\"large\" font_weight=\"bold\" color=\"#efaefb\">"
                        new-value "</span>"))
         (set self.txt.markup str))
       1 {:align "center"
          :markup (.. "<span size=\"large\" font_weight=\"bold\" color=\"#536452\">"
                      "Dirty Repos: </span>")
          :widget wibox.widget.textbox}
       2 {:id "txt"
          :widget wibox.widget.textbox}
       3 icons.fa-timeicon}))

(fn _G.update_repos_widget [repos]
  (when repos
    (: repos-widget.widget :set_count (tablex.size repos)))

  (local rows {:layout wibox.layout.fixed.vertical})

  (each [_ item (ipairs repos)]
    ;; TODO indicate diff summary?
    (table.insert rows (row item)))

  (: popup :setup rows))

(fn worker []
  (: repos-widget.widget :connect_signal "mouse::enter"
     (fn []
       (when (not popup.visible)
         (set popup.visible true)
         (: popup :move_next_to _G.mouse.current_widget_geometry))))

  (: repos-widget.widget :connect_signal "mouse::leave"
     (fn []
       (when popup.visible
         (set popup.visible false))))

  (: repos-widget.widget :buttons
     (awful.util.table.join
      (awful.button
       [] 1 (fn []
              (spawn.easy_async UPDATE_REPOS #(print "repos update requested"))))))

  (spawn.easy_async UPDATE_REPOS (fn [] (print "repos update requested")))

  repos-widget.widget)

(setmetatable repos-widget {:__call (fn [_ ...] (worker ...))})
