(local fun (require "fun"))
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")
(local naughty (require "naughty"))
(require-macros "macros")

(local hotkeys_popup (require "awful.hotkeys_popup"))


(local focus-colors
       ;; from https://colorhunt.co/palette/153787
       ;; ["#eb7070" "#fec771" "#e6e56c" "#64e291" ]
       ;; from https://colorhunt.co/palette/136945
       ["#cd4545" "#f16821" "#f3a333" "#fffe9a"])

(local unfocus-color "#000000")

(local focus-border-width 1)

(local background-color "#102")

;; Tools

(fn notify
  [text]
  (naughty.notify {:text text}))

(comment
 (notify "duder"))

(fn debug
  [x]
  (notify (gears.debug.dump_return x)))

(fn cset
  [k v]
  (fn [c]
    (tset c k v)))

(fn cinv
  [k]
  (fn [c]
    (let [v (. c k)]
      (tset c k (not v)))))

(local modifiers {:mod "Mod4"
                  :shift "Shift"
                  :ctrl "Control"})

(fn map-mods
  [mods]
  (->> mods
       (fun.map (partial . modifiers))
       (fun.totable)))

(fn key
  [mods key-code fun]
  (awful.key (map-mods mods) key-code fun))

(fn btn
  [mods btn-code fun]
  (awful.button (map-mods mods) btn-code fun))

(fn invert-screen
  [invert?]
  (os.execute "xcalib -c -a") ; clear
  (when invert?
    (os.execute "xcalib -i -a")))

(comment
 (invert-screen false))

;; Global keybindings

(local global-keys
       (gears.table.join
        (key [:mod :shift] "r" awesome.restart)
        (key [:mod]        "/" hotkeys_popup.widget.show_help)

        ;; (key [:mod :shift]       "a"         view-all-tags)
        ;; (key [:mod :shift]       "i"         toggle-invert-screen)
        ;; (key [:mod]              "h"         #(select-current-tag-by-offset [-1  0]))
        ;; (key [:mod]              "l"         #(select-current-tag-by-offset [ 1  0]))
        ;; (key [:mod]              "k"         #(select-current-tag-by-offset [ 0 -1]))
        ;; (key [:mod]              "j"         #(select-current-tag-by-offset [ 0  1]))
        ;; (key [:mod :ctrl]        "h"         #(move-all-clients-relative-and-select [-1  0]))
        ;; (key [:mod :ctrl]        "l"         #(move-all-clients-relative-and-select [ 1  0]))
        ;; (key [:mod :ctrl]        "k"         #(move-all-clients-relative-and-select [ 0 -1]))
        ;; (key [:mod :ctrl]        "j"         #(move-all-clients-relative-and-select [ 0  1]))
        ;; (key [:mod]              "space"     #(awful.layout.inc layouts  1))
        ;; (key [:mod :shift]       "space"     #(awful.layout.inc layouts -1))
        ;; (key [:mod :ctrl]        "space"     awful.client.floating.toggle)
        ;; (key [:mod]              "Tab"       #(focus-client-by-offset  1))
        ;; (key [:mod :shift]       "Tab"       #(focus-client-by-offset -1))
        ;; (key [:mod :ctrl]        "Tab"       #(awful.client.swap.byidx  1))
        ;; (key [:mod :shift :ctrl] "Tab"       #(awful.client.swap.byidx -1))
        ;; (key [:mod :shift]       "BackSpace" #(unminimize-tag (awful.tag.selected)))
        ))

(root.keys global-keys)
