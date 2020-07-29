(local awful (require "awful"))

(local scratchpad {})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create Client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn create-client
  [workspace]
  (let [emacs-file (. workspace :emacs-file)
        browser-url (. workspace :browser-url)]
    (if
     browser-url
     (awful.spawn
      (.. "google-chrome-stable --new-window " browser-url))

     emacs-file
     (awful.spawn.with_shell
      (.. "emacsclient --alternate-editor='' --no-wait --create-frame "
          emacs-file
          " -F '(quote (name . \""
          (. workspace :tag-name)
          "\"))' --display $DISPLAY")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle Scratchpad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn scratchpad.toggle
  [workspace]
  (fn []
    (let [tag-name (. workspace :tag-name)

          s (awful.screen.focused)
          x-tag (awful.tag.find_by_name s tag-name)
          x-client (when x-tag
                     (when (> (length (x-tag:clients)) 0)
                       (-> (x-tag:clients)
                           (. 1))))]
      (if
       ;; if tag and a client, toggle tag, focus client
       (and x-tag x-client)
       (do
         (awful.tag.viewtoggle x-tag)
         (tset x-client :ontop (not (. x-client :ontop)))
         (if (not x-client.active)
             ;; _G indicates a 'true' global, that fennel did not reject
             (tset _G.client :focus x-client)))

       ;; if tag but no client, create client
       (and x-tag (not x-client))
       (create-client workspace)

       ;; no tag? create it
       (not x-tag)
       (do
         (awful.tag.add tag-name {:screen s
                                  :gap 10})
         ;; TODO should only create here if no x-client exists
         ;; across whole system, not just in this tag
         (create-client workspace))
       ))))

scratchpad
