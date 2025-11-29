#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.git :as r.git]
  '[clojure.string :as string]
  '[clawe.config :as clawe.config]
  )

(defn log [msg] (r.hypr/notify (str "dirty_repo.bb: " msg)))

(defn wsp->is-repo?
  [def]
  (some-> def :workspace/directory r.git/dir-is-repo?))

(defn wsp->name [def] (:workspace/title def))

(defn repo-workspaces []
  (->>
    (clawe.config/workspace-defs-with-titles)
    (filter (fn [[_ def]] (wsp->is-repo? def)))
    (map second)))

(defn is-my-repo? [def]
  (let [path (:workspace/directory def)]
    (and path
         (or
           (string/includes? path "russmatney")
           (string/includes? path "dotfiles")))))

(defn my-repos []
  (->> (repo-workspaces) (filter is-my-repo?)))

(defn my-repo-statuses []
  (->> (my-repos)
       (map (fn [def] (merge def (-> def :workspace/directory r.git/status))))))

(defn notify-dirty
  [def]
  (when (seq (:git/dirty? def))
    (r.hypr/notify (str "fontsize:48 [" (wsp->name def) "]: Dirty!")
                   {:level :info :til 10000})))

(defn notify-needs-pull
  [def]
  (when (seq (:git/needs-pull? def))
    (r.hypr/notify (str "fontsize:48 [" (wsp->name def) "]: Needs Pull!")
                   {:level :error :til 15000})))

(defn notify-needs-push
  [def]
  (when (seq (:git/needs-push? def))
    (r.hypr/notify (str "fontsize:48 [" (wsp->name def) "]: Needs Push!")
                   {:level :warning :til 15000})))

(defn clean? [{:git/keys [needs-pull? needs-push? dirty?]}]
  (not (or dirty? needs-pull? needs-push?)))

(defn notify-clean
  [def]
  (when (clean? def)
    (r.hypr/notify (str "fontsize:24 [" (wsp->name def) "]: Clean!")
                   {:level :ok :til 5000})))

(defn notify-status
  "Not sure these all run..."
  [def]
  (notify-clean def)
  (notify-dirty def)
  (notify-needs-pull def)
  (notify-needs-push def))

(defn notify-dirty-repos []
  (let [defs (my-repo-statuses)]
    (run! notify-needs-pull defs)
    (run! notify-needs-push defs)
    (run! notify-dirty defs)
    (run! notify-clean defs)))

;; (update-my-repos)
(notify-dirty-repos)

(defn update-my-repos []
  (->>
    (my-repos)
    (map second)
    (map r.git/fetch-via-tmux)) )

(comment
  (notify-dirty-repos)
  (update-my-repos)
  )

