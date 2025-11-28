#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require '[ralphie.hyprland :as r.hypr])

(def window (r.hypr/get-active-window))
(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/title window) " " msg)))

;; TODO consider 'special' workspaces getting buried in the current one
;; need to then toggle the special space off
;; later, if it gets toggled back on, need to lift them back up to that special space

(cond
  (:special? workspace)
  (do
    (log "Special Workspace! Don't freak out!"))

  (:hypr/floating window)
  (do
    (log "Burying")
    (r.hypr/toggle-floating))

  (not (:hypr/floating window))
  (do
    (log "Lifting")
    (r.hypr/set-floating)))


