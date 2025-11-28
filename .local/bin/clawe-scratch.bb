#!/run/current-system/sw/bin/bb

(require '[clojure.string :as string]
         '[babashka.process :as p :refer [process]]
         '[babashka.tasks :as tasks]
         '[babashka.deps :as deps])

(def home "/home/russ")
(deps/add-deps
  {:deps {'russmatney/clawe
          {:local/root (str home "/russmatney/clawe")}}})

(require '[clawe.cli :as clawe-cli]
         '[ralphie.hyprland :as r.hypr]
         '[clawe.hyprland :as c.hypr])

(comment
  (clawe-cli/repo-dirs nil))

(comment
  (r.hypr/hc! "workspaces")
  (->
    (r.hypr/get-active-window)
    :hypr/title)

  (->
    (c.hypr/current-workspace nil)
    :workspace/title))
