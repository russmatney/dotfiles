;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local lume (require :lume))

(local mytags {})

(set mytags.slack-tag
     {:tag-name "slack"
      :apps [{:exec "slack"
              :classes ["Slack" "slack"]
              :names ["Slack" "slack"]}
             {:exec "discord"
              :classes ["Discord" "discord"]
              :names ["Discord" "discord"]}]})

(set mytags.spotify-tag
     {:tag-name "spotify"
      :apps [{:exec "spotify"
              :classes ["Spotify" "spotify"]
              :names ["Spotify" "spotify"]}
             {:exec "pavucontrol"
              :classes ["Pavucontrol" "pavucontrol"]
              :names ["Pavucontrol" "pavucontrol"]}]})

(set mytags.efb-tag
     {:tag-name "efb"
      :scratchpad-key "a"
      :emacs-file "~/.config/awesome/run-init.fnl"})

(set mytags.journal-tag
     {:tag-name "journal"
      :scratchpad-key "u"
      :emacs-file "~/todo/journal.org"})

(set mytags.notes-tag
     {:tag-name "notes"
      :scratchpad-key "r"
      :emacs-file "~/Dropbox/notes/readme.org"})

(set mytags.dotfiles-tag
     {:tag-name "dotfiles"
      :scratchpad-key "0"
      :emacs-file "~/dotfiles/readme.org"})

(set mytags.yodo-app-tag
     {:tag-name "yodo-app"
      :apps [{:exec ["/home/russ/.local/bin/start-yodo-electron.sh"]
              :names ["Yodo Electron"]}]})

(set mytags.yodo-dev-tag
     {:tag-name "yodo-dev"})

(set mytags.web-tag
     {:tag-name "web"
      ;; :browser-url "chrome://newtab"
      :browser-url "about:home"
      :except_any {:names ["localhost"]}
      :floating true
      :apps [{:classes ["firefox"]
              :names ["Mozilla Firefox" "firefox"]}]
      ;; :apps [{:classes ["Google-chrome"]
      ;;         :names ["Google-chrome" "google chrome" "Google Chrome"]}]
      })

;; NOTE order here determines order in bar
(local tag-list
       [mytags.efb-tag
        mytags.slack-tag
        mytags.spotify-tag
        mytags.web-tag
        mytags.notes-tag
        mytags.yodo-dev-tag
        mytags.dotfiles-tag
        {:tag-name "ralphie"}
        {:tag-name "org-crud"}
        mytags.journal-tag
        mytags.yodo-app-tag
        ])

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

(fn apps->match-rule
  [apps]
  (let [names (-> apps
                  (lume.map (fn [a] (. a :names)))
                  (lume.reduce lume.concat []))
        classes (-> apps
                    (lume.map (fn [a] (. a :classes)))
                    (lume.reduce lume.concat []))]
    (if
     (and (next names) (next classes))
     {:class classes :name names}
     (next names) {:name names}
     (next classes) {:class classes}
     )))

(local rules-apps-on-tag
       (-> tag-list
           (lume.filter
            (fn [t] (if (. t :apps) true false)))
           (lume.map
            (fn [t]
              (let [matcher (apps->match-rule t.apps)
                    except (apps->match-rule [t.except_any])
                    rule
                    {:rule_any matcher
                     :properties {:tag t.tag-name
                                  :floating t.floating}}]
                (if except
                    (do (tset rule :except_any except)
                        rule)
                    rule))))))


(comment
 (-> [{:apps [{:names ["one" "two"]}
              {:names ["three" "four"]}]}]
     ())

 (-> tag-list
     (lume.filter
      (fn [t] (if (. t :apps) true false)))
     (lume.map
      (fn [t]
        (let [matcher (apps->match-rule t.apps)
              except (apps->match-rule [t.except])
              rule
              {:rule_any matcher
               :properties {:tag t.tag-name
                            :floating false}}]
          (if except
              (do (tset rule :except except)
                  rule)
              rule)))))

 (let [y {}]
   (tset y :hi "bye")
   y)

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
