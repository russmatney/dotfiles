(local wibox (require "wibox"))
(local lume (require "lume"))

;; https://www.reddit.com/r/awesomewm/comments/kq79jn/awesomewm_icons_made_easy/

(fn make-icon [{: color : code : font : size}]
  (wibox.widget
   {:font (.. font " " size)
    :markup (.. " <span color='" color "'>" code "</span> ")
    :align :center
    :valign :center
    :widget wibox.widget.textbox}))

(local fa-icon-opts
       {:size 12
        :font "Font Awesome 5 Free-Solid-900"})

{:make-icon make-icon
 :make-fa-icon (fn [opts] (make-icon (lume.merge fa-icon-opts opts)))
 :fa-cpuicon (make-icon
              (lume.merge
               fa-icon-opts {:code "\u{f2db}"
                             :color "#587D8D"}))
 :fa-timeicon (make-icon
               (lume.merge
                fa-icon-opts {:code "\u{f017}"
                              :color "#587D8D"}))}
