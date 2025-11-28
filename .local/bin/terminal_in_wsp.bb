#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.tmux :as tmux])

(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/name workspace) " " msg)))

(log (str "Terminatin'"))
(tmux/open-session
  {:tmux/session-name (:hypr/name workspace)
   ;; :tmux/directory    "~/russmatney"
   })
