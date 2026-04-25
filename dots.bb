#!/usr/bin/env bb
;; dots.bb — Babashka symlink manager for dotfiles.
;;
;; Usage (standalone):  bb dots.bb [link|status|unlink] [--dry-run]
;; Usage (via bb task): bb dots   [link|status|unlink] [--dry-run]
;;
;; Commands:
;;   status    — print state of all declared links (default)
;;   link      — create missing symlinks; skip valid existing; warn conflicts
;;   unlink    — remove all managed symlinks
;;   --dry-run — (with link) print what would change without applying
;;
;; Link entry shapes in dots.edn:
;;   {:from "config/foot"  :to "~/.config/foot"}
;;     — symlinks the whole dir: ~/.config/foot -> <dotfiles>/config/foot
;;
;;   {:from "scripts"  :to "~/.local/bin"  :link-contents true}
;;     — symlinks each file inside scripts/ individually into ~/.local/bin/

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

;; ---------------------------------------------------------------------------
;; Paths

(def dotfiles-dir
  "Repo root: prefer $DOTFILES env, else derive from this script's location."
  (let [env (System/getenv "DOTFILES")]
    (if (and env (not (str/blank? env)))
      env
      (str (fs/parent (fs/absolutize (or *file* "dots.bb")))))))

(def home-dir (System/getenv "HOME"))

(def hostname
  (str/trim (slurp "/etc/hostname")))

(def config-path
  (str (fs/path dotfiles-dir "dots.edn")))

;; ---------------------------------------------------------------------------
;; Helpers

(defn expand-home [path]
  (if (str/starts-with? path "~/")
    (str home-dir (subs path 1))
    path))

(defn resolve-src [from]
  (str (fs/path dotfiles-dir from)))

(defn load-config []
  (if (fs/exists? config-path)
    (edn/read-string (slurp config-path))
    {:links []}))

(defn all-links [config]
  (concat (:links config [])
          (get-in config [:machines hostname] [])))

;; ---------------------------------------------------------------------------
;; Expand :link-contents entries into per-file link maps

(defn expand-link
  "If :link-contents true, returns one link map per file in :from dir.
   Otherwise returns the link map as-is in a single-element vector."
  [{:keys [from to link-contents] :as link}]
  (if-not link-contents
    [link]
    (let [src-dir (resolve-src from)
          dst-dir (expand-home to)]
      (if-not (fs/exists? src-dir)
        [link]  ;; will surface as :source-missing in status
        (->> (fs/list-dir src-dir)
             (filter fs/regular-file?)
             (sort-by str)
             (map (fn [f]
                    (let [fname (str (fs/file-name f))]
                      {:from          (str from "/" fname)
                       :to            (str to "/" fname)
                       :_link-contents-parent from}))))))))

(defn expand-all-links [links]
  (mapcat expand-link links))

;; ---------------------------------------------------------------------------
;; Link status (operates on already-expanded single links)

(defn link-status [{:keys [from to]}]
  (let [src (resolve-src from)
        dst (expand-home to)]
    (cond
      (not (fs/exists? src))
      {:status :source-missing
       :from from :to to :src src :dst dst
       :detail "source path does not exist in repo"}

      (fs/sym-link? dst)
      (if (= (str (fs/read-link dst)) src)
        {:status :ok      :from from :to to :src src :dst dst}
        {:status :conflict :from from :to to :src src :dst dst
         :detail (str "points elsewhere → " (fs/read-link dst))})

      (fs/exists? dst)
      {:status :conflict
       :from from :to to :src src :dst dst
       :detail "exists but is not a symlink — move it first"}

      :else
      {:status :missing :from from :to to :src src :dst dst})))

;; ---------------------------------------------------------------------------
;; Commands

(defn cmd-status []
  (let [config        (load-config)
        links         (expand-all-links (all-links config))]
    (if (empty? links)
      (do (println "No links configured.") (System/exit 0))
      (let [statuses      (mapv link-status links)
            has-problems? (some (comp #{:missing :conflict :source-missing} :status)
                                statuses)]
        (doseq [{:keys [status from dst detail]} statuses]
          (let [icon (case status
                       :ok             "✓"
                       :missing        "✗"
                       :conflict       "⚠"
                       :source-missing "?"
                       "?")]
            (println (format "%s  %-40s → %s%s"
                             icon from dst
                             (if detail (str "  [" detail "]") "")))))
        (System/exit (if has-problems? 1 0))))))

(defn cmd-link [dry-run?]
  (let [config (load-config)
        links  (expand-all-links (all-links config))]
    (if (empty? links)
      (println "No links configured.")
      (doseq [link links]
        (let [{:keys [status src dst detail]} (link-status link)]
          (case status
            :ok
            (println (format "✓  %-40s already linked" (:from link)))

            :missing
            (if dry-run?
              (println (format "→  %-40s would link → %s" (:from link) dst))
              (do
                (fs/create-dirs (fs/parent dst))
                (fs/create-sym-link dst src)
                (println (format "✓  %-40s → %s" (:from link) dst))))

            :conflict
            (println (format "⚠  %-40s skipping  [%s]" (:from link) detail))

            :source-missing
            (println (format "?  %-40s skipping  [%s]" (:from link) detail))))))))

(defn cmd-unlink []
  (let [config (load-config)
        links  (expand-all-links (all-links config))]
    (if (empty? links)
      (println "No links configured.")
      (doseq [link links]
        (let [{:keys [status dst]} (link-status link)]
          (if (= status :ok)
            (do
              (fs/delete dst)
              (println (format "✗  %-40s unlinked" (:from link))))
            (println (format "—  %-40s skipping (%s)" (:from link) (name status)))))))))

(defn print-usage []
  (println "Usage: bb dots [link|status|unlink] [--dry-run]")
  (println)
  (println "  status    print state of all declared links (default)")
  (println "  link      create missing symlinks")
  (println "  unlink    remove all managed symlinks")
  (println "  --dry-run (with link) show what would change without applying"))

;; ---------------------------------------------------------------------------
;; Dispatch

(let [[cmd & args] *command-line-args*
      dry-run?     (some #{"--dry-run" ":dry-run"} args)]
  (case cmd
    "status"  (cmd-status)
    "link"    (cmd-link dry-run?)
    "unlink"  (cmd-unlink)
    "help"    (print-usage)
    nil       (cmd-status)
    (do
      (println (str "Unknown command: " cmd))
      (println)
      (print-usage)
      (System/exit 1))))
