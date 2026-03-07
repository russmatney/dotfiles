#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr]
  '[ralphie.cache :as cache]
  '[clojure.edn :as edn])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn stack-cache-file []
  "Returns the path to the workspace stack cache file"
  (cache/cache-file "workspace-stack-cache.edn"))

(defn read-stack-cache []
  "Read the current stack state from cache, returns default if empty or on error"
  (try
    (let [file (stack-cache-file)
          raw (slurp file)]
      (if (empty? (clojure.string/trim raw))
        {:stack [] :active nil}
        (edn/read-string raw)))
    (catch Exception _e
      {:stack [] :active nil})))

(defn write-stack-cache [state]
  "Write the stack state to cache file"
  (spit (stack-cache-file) (pr-str state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-from-stack [stack wsp-name]
  "Remove a specific workspace from the stack"
  (filterv #(not= % wsp-name) stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace State Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-workspace-visible? [wsp-name]
  "Check if a special workspace is currently visible (not hidden)"
  (let [workspaces (r.hypr/list-workspaces)
        special-name (str "special:" wsp-name)]
    (->> workspaces
         (filter #(= (:hypr/name %) special-name))
         (some #(not (:hypr/hidden %))))))

(defn is-workspace-empty? [wsp-name]
  "Check if a special workspace has no windows/clients"
  (let [clients (r.hypr/list-clients)
        special-name (str "special:" wsp-name)]
    (->> clients
         (filter #(= (:workspace-name %) special-name))
         empty?)))

(defn clean-empty-workspaces [stack]
  "Remove all empty workspaces from the stack"
  (filterv #(not (is-workspace-empty? %)) stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Toggle Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn toggle-workspace-stack [wsp-name]
  "Toggle a special workspace with stack-based navigation"
  (let [{:keys [stack active]} (read-stack-cache)
        ;; Clean any empty workspaces from the stack first
        cleaned-stack (clean-empty-workspaces stack)
        ;; Update active if it was cleaned out
        cleaned-active (when (some #{active} cleaned-stack) active)]
    (cond
      ;; Scenario A: Toggle OFF the active workspace
      (= wsp-name cleaned-active)
      (let [new-stack (vec (butlast cleaned-stack))
            ;; Find the first non-empty workspace in the stack
            prev-wsp  (some #(when-not (is-workspace-empty? %) %)
                           (reverse new-stack))]
        (r.hypr/issue-dispatch "togglespecialworkspace" wsp-name)
        (when prev-wsp
          (r.hypr/issue-dispatch "togglespecialworkspace" prev-wsp))
        (write-stack-cache {:stack new-stack :active prev-wsp})
        (r.hypr/notify
          (if prev-wsp
            (str "← " prev-wsp)
            "Stack empty")
          {:level :info :til 2000}))

      ;; Scenario B/C: Toggle ON a workspace (new or buried)
      :else
      (let [new-stack (-> cleaned-stack
                          (remove-from-stack wsp-name)
                          (conj wsp-name))]
        (r.hypr/issue-dispatch "togglespecialworkspace" wsp-name)
        (write-stack-cache {:stack new-stack :active wsp-name})
        (r.hypr/notify
          (str "→ " wsp-name)
          {:level :ok :til 2000})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-not (seq *command-line-args*)
  (r.hypr/notify "Usage: toggle_workspace_stack.bb <workspace-name>" {:level :error :til 5000})
  (System/exit 1))

(def wsp-name (first *command-line-args*))
(toggle-workspace-stack wsp-name)
