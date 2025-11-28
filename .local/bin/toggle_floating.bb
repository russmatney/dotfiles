#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps
  {:deps {'russmatney/clawe
          {:local/root "/home/russ/russmatney/clawe"}}})

(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.notify :as r.notify])

(def window
  (r.hypr/get-active-window))

(defn log [msg]
  (r.notify/notify
    {:body    msg
     :subject "[TOGGLE FLOATING]"}))

;; TODO consider 'special' workspaces getting buried in the current one
;; need to then toggle the special space off
;; later, if it gets toggled back on, need to lift them back up to that special space

(when (:hypr/floating window)
  (log "Burying")
  (r.hypr/toggle-floating))

(when-not (:hypr/floating window)
  (log "Lifting")
  (r.hypr/set-floating))


