(local gears (require "gears"))
(local awful (require "awful"))
(local beautiful (require "beautiful"))

(local w (require :workspaces))
(local bindings (require :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         ;;    (ppi c)
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

         ;; attempt to wrangle browser windows
         ;; currently too broad, catching slack, discord, spotify
         {:rule {:role "browser"}
          :properties {:screen 1}
          :callback
          (fn [c]
            (if
             (and
              (not (= c.class "Slack"))
              (not c.name_change_handled))
             (do
               (var f nil)
               (set f
                    (fn [c]
                      (tset c :name_change_handled true)
                      (c:disconnect_signal "property::name" f)
                      (awful.rules.apply c)
                      (set c.minimized false)
                      ))
               (set c.minimized true)
               (c:connect_signal "property::name" f))))}
         ]

        w.rules-scratchpad-emacs
        w.rules-apps-on-tag))

(global
 init_rules
 (fn []
   (set awful.rules.rules global_rules)))
