#!/run/current-system/sw/bin/bb

(println (+ 1 2 3))

(def home "/home/russ")

(require '[babashka.process :as p :refer [process]]
         '[babashka.tasks :as tasks]
         '[babashka.deps :as deps]
         )

(tasks/shell {:dir home} "echo hi")

(->
  (process {:dir home :inherit true} "echo hey there")
  (p/check))


(deps/add-deps
  {:deps {'russmatney/clawe
          {:local/root (str home "/russmatney/clawe")}}})

(require '[clawe.cli :as clawe-cli])

(clawe-cli/repo-dirs nil)


