;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local lume (require :lume))

(local mytags {})

(set mytags.slack-tag
     {:sort-key 2
      :tag-name "slack"
      :apps [{:exec "slack"
              :classes ["Slack" "slack"]
              :names ["Slack" "slack"]}
             {:exec "discord"
              :classes ["Discord" "discord"]
              :names ["Discord" "discord"]}]})

(set mytags.spotify-tag
     {:sort-key 3
      :tag-name "spotify"
      :apps [{:exec "spotify"
              :classes ["Spotify" "spotify"]
              :names ["Spotify" "spotify"]}
             {:exec "pavucontrol"
              :classes ["Pavucontrol" "pavucontrol"]
              :names ["Pavucontrol" "pavucontrol"]}]})

(set mytags.awesome-tag
     {:sort-key 1
      :tag-name "awesome"
      :scratchpad-key "a"
      :emacs-file "~/.config/awesome/cfg.fnl"})

(set mytags.journal-tag
     {:sort-key 9
      :tag-name "journal"
      :scratchpad-key "u"
      :emacs-file "~/todo/journal.org"})

(set mytags.notes-tag
     {:sort-key 4
      :tag-name "notes"
      :scratchpad-key "r"
      :emacs-file "~/Dropbox/notes/readme.org"})

(set mytags.dotfiles-tag
     {:sort-key 0
      :tag-name "dotfiles"
      :scratchpad-key "0"
      :emacs-file "~/dotfiles/readme.org"})

(set mytags.yodo-tag
     {:sort-key 6
      :tag-name "yodo"
      :browser-url "http://localhost:4200"
      ;; "http://localhost:4222/devcards.html"
      })

(set mytags.web-tag
     {:sort-key 5
      :tag-name "web"
      :browser-url "chrome://newtab"})

;; NOTE order here determines order in bar
(local tag-list
       [mytags.awesome-tag
        mytags.slack-tag
        mytags.spotify-tag
        mytags.web-tag
        mytags.notes-tag
        mytags.yodo-tag
        mytags.journal-tag
        mytags.dotfiles-tag])

(local tag-names
       (lume.map tag-list
                 (fn [t]
                   (. t :tag-name))))

(local rules-scratchpad-emacs
       ;; for now, dodging awesome.placement.centered dep
       (-> tag-list
           (lume.filter
            (fn [t] (if (. t :scratchpad-key) true false)))
           (lume.map
            (fn [t]
              ;; tag named used to create emacs frame
              {:rule {:name t.tag-name}
               :properties
               {;; put it on the tag name
                :tag t.tag-name
                :screen 1
                :ontop true
                :above true
                :floating true
                :focus true}}))))

(local rules-apps-on-tag
       (-> tag-list
           (lume.filter
            (fn [t] (if (. t :apps) true false)))
           (lume.map
            (fn [t]
              (let [names (-> t.apps
                              (lume.map
                               (fn [a]
                                 (. a :names)))
                              (lume.reduce lume.concat))
                    classes (-> t.apps
                                (lume.map
                                 (fn [a] (. a :classes)))
                                (lume.reduce lume.concat))]
                {:rule_any {:class classes
                            :name names}
                 :properties {:tag t.tag-name
                              :floating false}})))))


(comment
 (-> [{:apps [{:names ["one" "two"]}
              {:names ["three" "four"]}]}]
     ())

 (lume.reduce [["one" "two"]
               ["three" "four"]]
              lume.concat)
 )

(lume.merge
 mytags
 {: tag-names
  : tag-list
  : rules-scratchpad-emacs
  : rules-apps-on-tag})
