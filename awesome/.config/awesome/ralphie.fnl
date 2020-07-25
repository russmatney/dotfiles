(local awful (require "awful"))

(fn cmd
  [command]
  (awful.spawn (.. "ralphie " command)))
