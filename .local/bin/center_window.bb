#!/run/current-system/sw/bin/bb

(println (+ 1 2 3))

(def home "/home/russ")

(require '[clojure.string :as string])

(require '[babashka.process :as p :refer [process]]
         '[babashka.tasks :as tasks])


(tasks/shell {:dir home} "echo hi")

(->
  (process {:dir home :inherit true} "echo hey there")
  (p/check))


;; dynamically adding clawe?!
(require '[babashka.deps :as deps])
(deps/add-deps
  {:deps {'russmatney/clawe
          {:local/root (str home "/russmatney/clawe")}}})

(require '[clawe.cli :as clawe-cli])

(clawe-cli/repo-dirs nil)

;; dynamically adding clawe via classpath?
;; (require '[babashka.classpath :as classpath]
;;          '[clojure.java.shell :refer [sh]])

;; (classpath/add-classpath
;;   (-> (sh "clojure" "-Spath" "-Sdeps"
;;           (str '{:deps {russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}}))
;;       :out string/trim))

(require '[ralphie.hyprland :as r.hypr])

(comment
  (r.hypr/hc! "workspaces")
  )


(defn hypr-dispatch [cmd]
  (->
    (process {:dir home :inherit true :out :string}
             cmd)
    (p/check)
    :out
    string/trim))

(comment
  (hypr-dispatch "echo hey there")
  )

(->
  (process {:dir home :inherit true}
           "echo hey there")
  (p/check))
