#!/run/current-system/sw/bin/bb

(require '[babashka.deps :as deps])
(deps/add-deps {:deps {'russmatney/clawe {:local/root "/home/russ/russmatney/clawe"}}})
(require
  '[ralphie.hyprland :as r.hypr])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Focus Cycling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-current-workspace-id []
  "Get the current workspace ID"
  (let [active-ws (r.hypr/active-workspace)]
    (:hypr/id active-ws)))

(defn get-workspace-windows [workspace-id]
  "Get all windows in the specified workspace, sorted by position"
  (->> (r.hypr/list-clients)
       (filter #(= (:hypr/workspace-id %) workspace-id))
       (filter #(not (:hypr/floating %))) ; Focus on tiled windows first
       (sort-by (juxt :hypr/at-x :hypr/at-y))))

(defn get-focused-window []
  "Get the currently focused window"
  (->> (r.hypr/list-clients)
       (filter :hypr/focus)
       first))

(defn cycle-window [direction]
  "Cycle focus to the next/prev window in the current workspace
   direction: :next or :prev"
  (let [current-ws-id (get-current-workspace-id)
        windows (get-workspace-windows current-ws-id)]
    (when (> (count windows) 1)
      (let [focused (get-focused-window)
            focused-addr (:hypr/address focused)
            window-addrs (mapv :hypr/address windows)
            current-idx (.indexOf window-addrs focused-addr)
            next-idx (case direction
                       :next (mod (inc current-idx) (count windows))
                       :prev (mod (dec current-idx) (count windows)))
            next-window (nth windows next-idx)]
        (r.hypr/dispatch (str "focuswindow address:" (:hypr/address next-window)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [direction (case (first *command-line-args*)
                  "prev" :prev
                  :next)]
  (cycle-window direction))
