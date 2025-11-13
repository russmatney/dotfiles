#!/usr/bin/env bb

(ns cycle-window-size
  (:require
   [babashka.process :refer [shell]]
   [clojure.string :as str]
   [cheshire.core :as json]))

(def window-state-file (str (System/getenv "HOME") "/.cache/window_size_state"))

;; Ensure state file exists
(when-not (.exists (java.io.File. window-state-file))
  (spit window-state-file "0"))

;; Get current state
(def current-state (Integer/parseInt (str/trim (slurp window-state-file))))

;; Get screen dimensions
(def monitor-info
  (-> (shell {:out :string} "hyprctl monitors -j")
      :out
      (json/parse-string true)
      first))

(def screen-width (:width monitor-info))
(def screen-height (:height monitor-info))

;; Calculate sizes for different modes
(def sizes
  [{:name   "Full screen"
    :width  screen-width
    :height screen-height
    :x      0
    :y      0}

   {:name   "90% screen"
    :width  (int (* screen-width 0.9))
    :height (int (* screen-height 0.9))
    :x      (int (/ (- screen-width (* screen-width 0.9)) 2))
    :y      (int (/ (- screen-height (* screen-height 0.9)) 2))}

   {:name   "Writing mode"
    :width  1200  ;; ~150 characters at 8px per character
    :height screen-height
    :x      (int (/ (- screen-width 1200) 2))
    :y      0}])

;; Calculate next state (0: full, 1: 90%, 2: writing mode)
(def next-state (mod (inc current-state) (count sizes)))

;; Apply the new window size and position
(let [target (get sizes next-state)]
  (println "Switching to" (:name target))

  ;; Resize window
  (let [res
        (shell {:out :string} (str "hyprctl dispatch resizewindowpixel exact "
                                   (:width target) " " (:height target)))]
    (println res)

    ;; Reposition window
    (println
      (shell {:out :string} (str "hyprctl dispatch movewindowpixel exact "
                                 (:x target) " " (:y target))))))

;; Save new state
(spit window-state-file (str next-state))

(println "Window size state updated to" next-state)
