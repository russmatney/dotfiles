#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps
  {:deps {'russmatney/clawe
          {:local/root "/home/russ/russmatney/clawe"}}})

(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.notify :as r.notify])

(defn log [msg]
  (r.notify/notify
    {:body    msg
     :subject "[CYCLE WINDOW POSITIONS]"}))

(comment
  (->
    (r.hypr/get-active-window)
    :hypr/title))

;; force float
(r.hypr/set-floating)
(r.hypr/resize-client {:x "90%" :y "90%"})
(r.hypr/center-client)
(r.hypr/resize-client {:x "70%" :y "70%"})
(r.hypr/move-client {:relative? true :x "360" :y "0"})

(log "POSITION1")

(comment
  (r.hypr/hc! "workspaces"))

