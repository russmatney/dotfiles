(local naughty (require "naughty"))

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
