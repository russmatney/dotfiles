;; named `run-init` rather than `init` to prevent accidental lua module loading


;; TODO wrap side effects of these requires
;; REPL env
(require "./remote")
;; top/bottom bar
(require "./bar")

(global
 init_awesome
 (fn []
   (print "\n\ninit_theme\n")
   (init_theme)
   (print "\n\ninit_error_handling\n")
   (init_error_handling)
   (print "\n\ninit_tags\n")
   (init_tags)
   (print "\n\nset_rules\n")
   (set_rules)
   (print "\n\ninit_manage_signal\n")
   (init_manage_signal)
   (print "\n\ninit_request_titlebars\n")
   (init_request_titlebars)
   (print "\n\ninit_focus_signals\n")
   (init_focus_signals)
   (print "\n\ninit_arrange_signal\n")
   (init_arrange_signal)
   (print "\n\ninit_spawns\n")
   (init_spawns)))
