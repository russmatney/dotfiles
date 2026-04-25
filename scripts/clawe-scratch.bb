#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require '[ralphie.hyprland :as r.hypr])

(def window (r.hypr/get-active-window))
(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/title window) " " msg)))

(comment
  (->
    (r.hypr/get-active-window)
    :hypr/title)
  )
