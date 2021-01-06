(local gears (require "gears"))
(local awful (require "awful"))
(local beautiful (require "beautiful"))

(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prevent matching clients from stealing focus
;; not sure why this isn't handled by the rules...
;; (awful.ewmh.add_activate_filter
;;  (fn [c ctx _hints]
;;    ;; (print (.. ctx " activation filter requested"))
;;    ;; (print c.class)
;;    ;; (print c.name)
;;    ;; (pp hints)
;;    (when (or (= ctx "rules")
;;              (= ctx "ewmh"))
;;      (if (= c.class "love") false))))

;; Rules to apply to new clients (through the "manage" signal).
(local global_rules
       (gears.table.join
        [{:rule {}
          :properties
          {:border_width beautiful.border_width
           :border_color beautiful.border_normal
           :focus awful.client.focus.filter
           :raise true
           :keys bindings.clientkeys
           :buttons bindings.clientbuttons
           :size_hints_honor false ;; Remove gaps between terminals
           :screen awful.screen.preferred
           :callback awful.client.setslave
           :placement (+ awful.placement.no_overlap
                         awful.placement.no_offscreen)}}

         ;; {:rule {}
         ;;  :callback
         ;;  (fn [c]
         ;;    (print "\n\nnew client!")
         ;;    (pp c)
         ;;    (print c.class)
         ;;    (print c.name)
         ;;    )}

         ;; Floating clients.
         {:rule_any
          {:instance ["DTA" "copyq"]
           :class ["Arandr" "MessageWin" "Sxiv" "Wpa_gui"
                   "pinentry" "veromix" "xtightvncviewer"]
           :role ["pop-up"]} ;; e.g. Google Chrome's (detached) Developer Tools.
          :properties {:floating true}}

         ;; Add titlebars to normal clients and dialogs
         {:rule_any {:type ["normal" "dialog"]}
          :properties {:titlebars_enabled true}}

         ;; handle status bar
         {:rule_any {:name ["yodo/app"]}
          :properties {:tag "yodo-app"
                       :titlebars_enabled true}}

         ;; handle status bar
         {:rule_any {:name ["yodo/bar"]}
          :properties {:titlebars_enabled false
                       :floating true
                       :sticky true
                       :focusable false
                       :maximized_horizontal true
                       :height 100
                       :x 0
                       :y 0}
          :callback (fn [c]
                      ;; remove from any tags
                      (c:tags {}))}

         ;; handle org protocol/emacs popups
         {:rule_any {:name ["org-capture-frame" "doom-capture"]}
          :properties {
                       ;; :titlebars_enabled false
                       :floating true
                       :ontop true
                       :width     1600
                       :height    800
                       :placement awful.placement.centered}}

         {:rule_any {:class ["workrave" "Workrave"
                             "Rest break" "Micro break"]
                     :name ["workrave" "Workrave"
                            "Rest break" "Micro break"]}
          :properties {:floating true
                       :ontop true
                       :above true
                       :sticky true
                       :switch_to_tags true
                       :tag "workrave"
                       :new_tag "workrave"}}

         {:rule {:class "firefox"}
          :properties {:tag "web"
                       :maximized false
                       :floating false}}

         ;; youtube fix
         {:rule {:instance "plugin-container"}
          :properties {:floating true}}
         {:rule {:instance "_NET_WM_STATE_FULLSCREEN"}
          :properties {:floating true}}
         {:rule {:instance "exe"}
          :properties {:floating true}}

         {:rule {:name "notes"}
          :properties {:tag "notes"}}
         {:rule {:name "journal"}
          :properties {:tag "journal"}}
         {:rule {:name "ralphie"}
          :properties {:tag "ralphie"}}
         {:rule {:name "org-crud"}
          :properties {:tag "org-crud"}}
         ;; TODO support arbitrary name <> tag for repos

         {:rule_any {:class ["Spotify" "spotify" "Pavucontrol" "pavucontrol"]
                     :name ["Spotify" "spotify" "Pavucontrol" "pavucontrol"]}
          :properties {:tag "spotify"
                       :new_tag "spotify"
                       :switch_to_tags true
                       :first_tag "spotify"}}

         {:rule_any {:class ["Slack" "slack" "Discord" "discord"]
                     :name ["Slack" "slack" "Discord" "discord"]}
          :properties {:tag "slack"
                       :new_tag "slack"
                       :first_tag "slack"
                       :switch_to_tags true}}]))

(set
 _G.init_rules
 (fn []
   (set awful.rules.rules global_rules)))
