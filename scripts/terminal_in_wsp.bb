#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.tmux :as tmux]
  '[clawe.config :as clawe.config])

(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/name workspace) " " msg)))

(def clawe-wsps (clawe.config/workspace-defs-with-titles))
(def clawe-wsp (clawe-wsps (:hypr/name workspace)))

(log (str "Terminatin'"))
(tmux/open-session
  {:tmux/session-name (:hypr/name workspace)
   :tmux/directory    (:workspace/directory clawe-wsp)})
