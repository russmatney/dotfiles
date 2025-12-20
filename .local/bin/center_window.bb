#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require '[ralphie.hyprland :as r.hypr])

(try
  (def window (r.hypr/get-active-window))
  (def workspace (r.hypr/get-active-workspace))

  (catch Exception err
    (do
      (r.hypr/notify "error centering!")
      (println err))))

(defn log [msg] (r.hypr/notify (str (:hypr/title window) " " msg)))

(comment
  (->
    (r.hypr/get-active-window)
    :hypr/tags)

  (r.hypr/add-tag-to-current "hiya")
  (r.hypr/get-current-tags))

;; force float
(r.hypr/set-floating)
(r.hypr/resize-client {:x "90%" :y "90%"})
(r.hypr/center-client)

;; TODO provide positions per client via clawe config.edn?
;; or load 'em all up in here?
(def positions
  {"pos0" {:resize {:x "70%" :y "70%"}
           :move   {:x "900" :y "0" :relative? true}
           :next   "pos1"}
   "pos1" {:resize {:x "70%" :y "70%"}
           :move   {:x "360" :y "0" :relative? true}
           :next   "pos2"}
   "pos2" {:resize {:x "85%" :y "85%"}
           :move   {:x "180" :y "0" :relative? true}}})

(defn move-to-pos [[k opts]]
  (log k)
  (r.hypr/resize-client (:resize opts))
  (r.hypr/move-client (:move opts)))

(def curr-tags (r.hypr/get-current-tags))
(def current-pos (->> positions keys (filter curr-tags) first))
(def next-key (some-> current-pos positions :next))
(def next-key (or next-key (some-> positions ffirst)))
(def next-opts (positions next-key))

(r.hypr/add-tag-to-current next-key)
  (r.hypr/remove-tag-from-current current-pos)
  (move-to-pos [next-key next-opts])

(comment
  (->> positions keys (filter (fn [k] ((r.hypr/get-current-tags) k))) first)
  (->> positions keys)
  (r.hypr/hc! "workspaces"))

