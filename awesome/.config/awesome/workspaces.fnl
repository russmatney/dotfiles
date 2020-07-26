


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local slack
       {:name "Chat"
        :apps [{:name "Slack"
                :exec "slack"}
               "discord"]
        :layout :fair})

(local spotify
       {:name "music"
        :apps [{:name "Spotify"}
               {:name "Pavucontrol"}]
        :layout :unfair})

(fn create-emacs-client [emacs-app]
  (let [file-name emacs-app.file
        frame-name emacs-app.title])
  (.. "emacsclient --alternate-editor='' --no-wait --create-frame "
      file-name
      " -F '(quote (name . \""
      frame-name
      "\"))' --display $DISPLAY"))

(local notes
       {:name "Notes"
        :apps [{:name "Emacs"
                :exec create-emacs-client
                :title "journal"
                :file "~/Dropbox/notes/index.org"}]})

(local journal
       {:name "Journal"
        :apps [{:name "Emacs"
                :title "journal"
                :file "~/todo.journal.org"
                :exec create-emacs-client}]})

(local yodo
       {:name "yodo-dev"
        :apps [{:name "Emacs"
                :title "yodo"
                :file "~/russmatney/yodo/deps.edn"
                :exec create-emacs-client}]})

(local workspace-tags
       [chat
        spotify
        notes
        journal
        yodo

        ])

(fn create-app [tag-client]
  (println tag-client))
(fn create-apps [tag-client]
  )
(fn create-apps [tag-clients]
  )
