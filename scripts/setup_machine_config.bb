#!/run/current-system/sw/bin/bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine-specific Configuration Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def home (System/getenv "HOME"))
(def hostname (str/trim (slurp "/etc/hostname")))

(defn setup-hypr-machine-config []
  "Setup machine-specific Hyprland configuration symlinks"
  (let [hypr-config-dir (str home "/.config/hypr")
        machine-dir     (str home "/dotfiles/hypr/machines/" hostname)
        monitors-link   (str hypr-config-dir "/monitors-machine.conf")
        overrides-link  (str hypr-config-dir "/overrides-machine.conf")]

    (when (fs/exists? machine-dir)
      ;; Create symlink for monitors
      (when (fs/exists? (str machine-dir "/monitors.conf"))
        (when (fs/exists? monitors-link)
          (fs/delete monitors-link))
        (fs/create-sym-link monitors-link (str machine-dir "/monitors.conf"))
        (println (str "✓ Linked monitors config for " hostname)))

      ;; Create symlink for overrides
      (when (fs/exists? (str machine-dir "/overrides.conf"))
        (when (fs/exists? overrides-link)
          (fs/delete overrides-link))
        (fs/create-sym-link overrides-link (str machine-dir "/overrides.conf"))
        (println (str "✓ Linked overrides config for " hostname))))))

(defn main []
  (println (str "Setting up machine-specific config for: " hostname))
  (setup-hypr-machine-config)
  (println "\nDone! Reload Hyprland to apply changes."))

(main)
