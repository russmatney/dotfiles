(pcall "require" "luarocks.loader")

(local awful (require "awful"))
(local naughty (require "naughty"))
(local beautiful (require "beautiful"))

(local w (require :workspaces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_error_handling
 (fn []
   ;; Check if awesome encountered an error during startup and fell back to
   ;; another config (This code will only ever execute for the fallback config)
   (if _G.awesome.startup_errors
       (naughty.notify
        {:preset  naughty.config.presets.critical
         :title  "Oops, there were errors during startup!"
         :text  _G.awesome.startup_errors }))

   ;; TODO fix the default config's bg, bindings, etc
   ;; TODO fix error handling ux

   ;; Handle runtime errors after startup
   (let []
     (var in_error false)
     (_G.awesome.connect_signal
      "debug::error"
      (fn [err]
        ;; Make sure we don't go into an endless error loop
        (if (not in_error)
            (set in_error true)
            (naughty.notify {:preset naughty.config.presets.critical
                             :title "Oops, an error happened!"
                             :text (tostring err) })
            (set in_error false)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global (External) Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 reapply_rules
 (fn []
   (each [c (awful.client.iterate (fn [_] true))]
     (awful.rules.apply c))))

(global
 set_layout
 (fn [layout]
   (awful.layout.set layout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global
 init_tags
 (fn [config]
   (when (and config (. config :tag_names))
     (print "found tag_names in config")
     (pp config.tag_names))

   (let [tag-names (or (and config config.tag_names) w.tag-names)]
     (each [_ tag-name (pairs tag-names)]
       (let [existing-tag (-> (awful.screen.focused)
                              (awful.tag.find_by_name tag-name))]
         (print (..  "Tag for " tag-name "?"))
         (pp existing-tag)
         (when (not existing-tag)
           (awful.tag.add tag-name {:layout (. layouts 1)})))))

   (when (and config (. config :tag_names))
     (reapply_rules))
   ))
