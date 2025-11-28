#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.git :as r.git]
  '[clojure.string :as string]
  '[clawe.config :as clawe.config]
  )

(defn log [msg] (r.hypr/notify (str "Dirty Repo! " msg)))

(defn wsp->is-repo? [def]
  (some-> def :workspace/directory r.git/dir-is-repo?))

(defn repo-dirs []
  (->>
    (clawe.config/workspace-defs)
    (filter (fn [[_ def]] (wsp->is-repo? def)))
    (map (fn [[k def]] [k (:workspace/directory def)]))))

(defn my-repo-dirs []
  (->>
    (clawe.config/workspace-defs)
    (filter (fn [[_ def]] (wsp->is-repo? def)))
    (filter (fn [[_ def]]
              (or
                (string/includes? (:workspace/directory def) "russmatney")
                (string/includes? (:workspace/directory def) "dotfiles"))))
    (map (fn [[k def]]
           [k
            (-> def
                :workspace/directory
                r.git/status
                )]))
    ;; (map str)
    ;; (string/join "\n")
    (into {})
    ))

(comment
  (my-repo-dirs)
  )
