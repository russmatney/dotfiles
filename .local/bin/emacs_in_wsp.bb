#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.emacs :as emacs])

;; (def window (r.hypr/get-active-window))
(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify (str (:hypr/name workspace) " " msg)))

(log (str "Emacsin'"))
(emacs/open {:workspace/title (:hypr/name workspace)})

