#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.emacs :as emacs]
  '[clawe.config :as clawe.config])

;; (def window (r.hypr/get-active-window))
(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/name workspace) " " msg)))
(log (str "Emaxin'"))

(def clawe-wsps (clawe.config/workspace-defs-with-titles))
(def clawe-wsp (clawe-wsps (:hypr/name workspace)))

(if clawe-wsp
  (emacs/open clawe-wsp)
  (emacs/open {:emacs.open/title (:hypr/name workspace)}))


