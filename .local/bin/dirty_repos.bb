#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.git :as r.git]
  '[ralphie.bb :as r.bb]
  '[ralphie.zsh :as r.zsh]
  '[clojure.string :as string]
  '[clawe.config :as clawe.config]
  '[babashka.process :refer [$]]
  )

(defn log [msg] (r.hypr/notify (str "dirty_repo.bb: " msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced git status checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-remotes [repo-path]
  "Get all git remotes for a repo with their URLs"
  (try
    (let [output (r.bb/run-proc
                   {:error-message (str "Error getting remotes for " repo-path)}
                   ^{:dir (r.zsh/expand repo-path)}
                   ($ git remote -v))]
      (->> output
           (map #(when-let [[_ name url type] (re-matches #"(\S+)\s+(\S+)\s+\((\w+)\)" %)]
                   {:remote/name name
                    :remote/url url
                    :remote/type type}))
           (filter some?)
           (filter #(= "push" (:remote/type %)))))
    (catch Exception _e [])))

(defn has-codeberg-remote? [remotes]
  "Check if any remote points to codeberg"
  (some #(string/includes? (:remote/url %) "codeberg") remotes))

(defn get-preferred-remote [repo-path]
  "Get preferred remote, preferring codeberg over others"
  (let [remotes (get-remotes repo-path)]
    (if (has-codeberg-remote? remotes)
      (first (filter #(string/includes? (:remote/url %) "codeberg") remotes))
      (first (filter #(= "origin" (:remote/name %)) remotes)))))

(defn get-detailed-status [repo-path remote-name]
  "Get detailed git status including commit counts"
  (try
    (let [output (r.bb/run-proc
                   {:error-message (str "Error getting detailed status for " repo-path)}
                   ^{:dir (r.zsh/expand repo-path)}
                   ($ git status -sb))]
      (when (seq output)
        (let [first-line (first output)
              ahead-match (re-find #"ahead (\d+)" first-line)
              behind-match (re-find #"behind (\d+)" first-line)]
          {:git/ahead-count (when ahead-match (parse-long (second ahead-match)))
           :git/behind-count (when behind-match (parse-long (second behind-match)))})))
    (catch Exception _e {})))

(defn get-dirty-file-count [repo-path]
  "Count number of dirty files"
  (try
    (let [output (r.bb/run-proc
                   {:error-message (str "Error getting dirty files for " repo-path)}
                   ^{:dir (r.zsh/expand repo-path)}
                   ($ git status --porcelain))]
      (count output))
    (catch Exception _e 0)))

(defn time-since-fetch [timestamp]
  "Calculate time since last fetch in human readable format"
  (when timestamp
    (let [instant (if (instance? java.nio.file.attribute.FileTime timestamp)
                    (.toInstant timestamp)
                    timestamp)
          now (java.time.Instant/now)
          duration (java.time.Duration/between instant now)
          hours (.toHours duration)
          days (.toDays duration)]
      (cond
        (> days 7) (str days " days")
        (> days 1) (str days " days")
        (> hours 1) (str hours " hours")
        :else "< 1 hour"))))

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
       (map (fn [def]
              (let [repo-path (:workspace/directory def)
                    base-status (r.git/status repo-path)
                    remote (get-preferred-remote repo-path)
                    detailed (get-detailed-status repo-path (:remote/name remote))
                    dirty-count (get-dirty-file-count repo-path)]
                (merge def base-status detailed
                       {:git/preferred-remote remote
                        :git/dirty-file-count dirty-count}))))))

(defn notify-dirty
  [def]
  (when (seq (:git/dirty? def))
          (let [file-count (:git/dirty-file-count def)
              msg (str "fontsize:24 [" (wsp->name def) "]: Dirty! "
                       file-count " file" (when (> file-count 1) "s") " modified")]
      (r.hypr/notify msg {:level :info :til 10000}))))

(defn notify-needs-pull
  [def]
  (when (seq (:git/needs-pull? def))
          (let [behind-count (:git/behind-count def)
              msg (str "fontsize:24 [" (wsp->name def) "]: Needs Pull!"
                       (when behind-count
                         (str " " behind-count " commit" (when (> behind-count 1) "s") " behind")))]
      (r.hypr/notify msg {:level :error :til 15000}))))

(defn notify-needs-push
  [def]
  (when (seq (:git/needs-push? def))
    (let [ahead-count (:git/ahead-count def)
          remote (:git/preferred-remote def)
          remote-name (or (:remote/name remote) "origin")
          remote-indicator (when (and remote (string/includes? (:remote/url remote) "codeberg"))
                            " → codeberg")
          msg (str "fontsize:24 [" (wsp->name def) "]: Needs Push!" remote-indicator
                   (when ahead-count
                     (str " " ahead-count " commit" (when (> ahead-count 1) "s") " ahead of " remote-name)))]
      (r.hypr/notify msg {:level :warning :til 15000}))))

(defn notify-stale-fetch
  [def]
  (let [last-fetch (:git/last-fetch-timestamp def)
        time-ago (time-since-fetch last-fetch)]
    (when (and last-fetch time-ago)
      (let [instant (if (instance? java.nio.file.attribute.FileTime last-fetch)
                      (.toInstant last-fetch)
                      last-fetch)
            duration (java.time.Duration/between instant (java.time.Instant/now))
            days (.toDays duration)]
        (when (> days 3)
          (let [msg (str "fontsize:24 [" (wsp->name def) "]: Stale! Last fetch: " time-ago " ago")]
            (r.hypr/notify msg {:level :warning :til 12000})))))))

(defn clean? [{:git/keys [needs-pull? needs-push? dirty?]}]
  (not (or dirty? needs-pull? needs-push?)))

(defn notify-clean
  [def]
  (when (clean? def)
    (let [last-fetch (:git/last-fetch-timestamp def)
          time-ago (time-since-fetch last-fetch)
          remote (:git/preferred-remote def)
          remote-indicator (when (and remote (string/includes? (:remote/url remote) "codeberg"))
                            " (codeberg)")
          msg (str "fontsize:24 [" (wsp->name def) "]: Clean!" remote-indicator
                   (when time-ago (str " Last fetch: " time-ago " ago")))]
      (r.hypr/notify msg {:level :ok :til 5000}))))

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
    (run! notify-stale-fetch defs)
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
