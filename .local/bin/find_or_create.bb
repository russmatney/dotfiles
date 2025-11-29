#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[clawe.config :as clawe.config]
  '[clawe.client.create :as client.create]
  )

(def workspace (r.hypr/get-active-workspace))
(defn log [msg] (r.hypr/notify msg))
(defn log-error [msg] (r.hypr/notify msg {:level :error :til 10000}))

(def clawe-wsps (clawe.config/workspace-defs-with-titles))
(def clawe-wsp (clawe-wsps (:hypr/name workspace)))

(when-not (seq *command-line-args*)
  (log "No args passed to find_or_create.bb"))

(def app (first *command-line-args*))
(def clawe-client (clawe.config/client-def app))

;; search for client by tag
;; if found, check workspace
;; if needed, move it to the special workspace
;; if not found, create it

(log (str "Creating: " clawe-client))
(try
  (client.create/create-client clawe-client)
  (catch Exception e
    (log-error (str "Error creating client: " e))
    (println e)))

