;;;  -*- lexical-binding: t; -*-

(require 'doct)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(use-package! org-rich-yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings

;; save hooks
(advice-add 'org-archive-subtree
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-refile
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-agenda-redo :after 'org-save-all-org-buffers)

(after! org 'turn-on-auto-fill)

;; monthly archives
(setq org-archive-location
      (concat "~/Dropbox/todo/archive/"
              (format-time-string "%Y-%m") ".org::"))

;; todo keywords
(setq org-todo-keywords
      '((sequence
         "[ ](T)"                       ; A task that needs doing
         "[-](S)"                       ; Task is in progress
         "[?](W)"                       ; Task is being held up or paused
         "|"
         "[X](D)"      ; Task was completed
         "(B)"      ; no longer a todo
         )
        (sequence
         "TODO(t)"               ; A task that needs doing & is ready to do
         "STRT(s)"               ; A task that is in progress
         "WAIT(w)"               ; Something external is holding up this task
         "HOLD(h)"               ; This task is paused/on hold because of me
         "STREAM(r)"
         "MTG(m)"
         "PILL(p)"
         "CHORE(c)"
         "|"
         "DONE(d)"      ; Task successfully completed
         "SKIP(k)"     ; Skipped a recurring task
         "(b)"     ; no longer a todo
         ))
      org-todo-keyword-faces
      '(("[-]" . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("MTG" . +org-todo-active)
        ("STREAM" . +org-todo-active)
        ("CHORE" . +org-todo-active)
        ("PILL" . +org-todo-active)
        ("[?]" . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold))

      org-global-properties
      '(("Effort_ALL" . "1 2 3 5 8 13 21 34 55")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Agenda

(setq org-log-done 'time
      ;; org-log-done 'note ;; <-- an interesting option
      org-agenda-log-mode-items '(closed clock state)

      org-agenda-time-grid
      '((daily today require-timed remove-match)
        (900 1700)
        "......"
        "----------------")
      org-agenda-show-current-time-in-grid nil

      ;; don't show completed items in the agenda
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-garden-files
      (append (file-expand-wildcards "~/Dropbox/todo/garden/*.org")))

(setq org-agenda-files
      (cl-remove-if
       (lambda (s)
         (or
          ;; (s-contains? "icebox" s)
          (s-contains? "goals" s)
          (s-contains? "ideas" s)
          (s-contains? "urbint" s)
          ;; (s-contains? "prompts" s)
          (s-contains? "reads" s)
          (s-contains? "watches" s)))
       (file-expand-wildcards "~/Dropbox/todo/*.org")))

(setq org-agenda-custom-commands
      '(;; TODO note that this misses items scheduled beyond the current agenda
        ("n" "Agenda and unscheduled TODOs"
         ((agenda "") (alltodo "" ((org-agenda-todo-ignore-with-date t)))))
        ("i" "Icebox"
         ((alltodo "" ((org-agenda-files (file-expand-wildcards "~/todo/icebox.org"))))))
        ("f" "Food"
         ((agenda "" ((org-agenda-files (file-expand-wildcards "~/todo/food.org"))))))
        ("g" "Garden"
         ((agenda "" ((org-agenda-files org-garden-files)))
          (alltodo "" ((org-agenda-files org-garden-files)))))
        ("d" "Today's items" agenda ""
         ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "0d")))
        ("D" "Today's items (export)" agenda ""
         ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "0d"))
         "~/todo/daily-agenda.html")
        ("u" "stand [u]p - all tasks yesterday, today, tomorrow" agenda ""
         ((org-agenda-span 3)
          (org-agenda-start-day "-1d")
          (org-agenda-skip-scheduled-if-done nil)
          (org-agenda-skip-deadline-if-done nil)
          (org-agenda-skip-scheduled-if-deadline-is-shown nil)
          (org-agenda-start-with-log-mode '(clock state))
          (org-agenda-archives-mode t)))))

;; https://www.reddit.com/r/orgmode/comments/grgzlb/display_file_path_in_agenda_view/
(defun my-buffer-dir-name ()
  "Give the directory of (buffer-file-name), and replace the home path by '~'"
  (interactive)
  (if (buffer-file-name)
      (->>
       (file-name-directory
        (file-relative-name
         (buffer-file-name)
         (expand-file-name "~")))
       (s-replace "Library/CloudStorage/" "")
       (s-replace "Dropbox/todo/" "")
       (s-replace "russmatney/" "")
       (s-replace-regexp "/$" ""))
    ""))

(setq org-agenda-prefix-format
      '((agenda  . " %-12(my-buffer-dir-name)%?i%-12:c%?-12t% s")
        (timeline  . "%?(my-buffer-dir-name)% s")
        (todo  . " %?-24(my-buffer-dir-name)%?-12:c")
        (tags  . "%?(my-buffer-dir-name)%i %-12:c")
        (search . "%?(my-buffer-dir-name)%i %-12:c")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org refile

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      org-outline-path-complete-in-steps nil

      ;; startup folded by default (overwritable per file)
      org-startup-folded t)

(defun russ/reset-refile-targets ()
  (setq
   org-todo-targets (file-expand-wildcards "~/Dropbox/todo/*.org")
   org-journal-archive-targets (file-expand-wildcards "~/Dropbox/todo/journal/*.org")
   org-dailies-targets (file-expand-wildcards "~/Dropbox/todo/daily/*.org")

   org-refile-targets
   '((org-journal-archive-targets :maxlevel . 1)
     (nil :maxlevel . 9)
     (org-todo-targets :maxlevel . 2)
     (org-dailies-targets :maxlevel . 1))))

(russ/reset-refile-targets)

;; from https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun russ/refile-to (file headline)
  "Move current headline to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))
    (switch-to-buffer (current-buffer))))

(defun russ/refile-to-daily (n)
  ;; TODO this seems to sometimes nest the `daily/` an extra time :/
  (let ((file-s (format-time-string "~/todo/daily/%Y-%m-%d.org" (time-add (* n 86400) (current-time)))))
    (save-excursion
      (org-roam-dailies-capture-tomorrow n t))
    ;; TODO write the file?
    (russ/refile-to file-s "new")))

(comment
 (format-time-string "%Y-%m-%d.org" (current-time))
 (format-time-string "%Y-%m-%d.org" (time-add 86400 (current-time)))
 (format-time-string "%Y-%m-%d.org" (time-add (* (- 1) 86400) (current-time))))

(defhydra hydra-org-refile-daily (:exit t)
  ("t" (russ/refile-to-daily 0) "Today")
  ("y" (russ/refile-to-daily -1) "Yesterday")
  ("T" (russ/refile-to-daily 1) "Tomorrow")
  ("j" russ/org-refile-to-daily-note "To some daily note" :column "Filter"))

(defhydra hydra-org-refile (:exit t)
  ("r" org-refile "Org refile" :column "~/todo")
  ("f" +org/refile-to-file "Refile to File")
  ("t" (russ/refile-to "~/todo/projects.org" "Todos") "project.org/Todos")
  ("h" (russ/refile-to "~/todo/projects.org" "Hammock") "project.org/Hammock")
  ("i" (russ/refile-to "~/todo/icebox.org" "new") "To icebox.org")
  ("p" (russ/refile-to "~/todo/principles.org" "new") "To principles.org")

  ("g" russ/org-refile-to-existing-note "To existing note" :column "garden")
  ("c" russ/org-refile-to-new-note "Create new note")
  ("d" hydra-org-refile-daily/body "To some daily note")
  ("w" russ/org-refile-to-workspace-note "To some workspace note")
  ("b" russ/org-refile-to-bucket-note "To a bucket note, i.e. ideas/writing accumulation files"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture

(after! org-capture
  (map! :map org-capture-mode-map
        [remap evil-save-and-close]          #'org-capture-finalize
        [remap evil-save-modified-and-close] #'org-capture-finalize
        [remap evil-quit]                    #'org-capture-kill))

(after! org-capture
  (setq org-capture-templates
        (doct '(("journal"
                 :keys "j"
                 :file "~/todo/journal.org"
                 :template ("* %?"))
                ("Garden Daily"
                 :keys "d"
                 :function org-roam-dailies-capture-today)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(after! org
  (map! :map org-mode-map
        "M-v"   #'evil-paste-after
        "M-RET" #'org-insert-item
        "M-t"   #'org-set-tags-command
        "M-T"   #'org-insert-todo-subheading
        [tab]   #'+org/toggle-fold
        "M-o"   #'+org/insert-item-below
        "M-O"   #'+org/insert-item-above
        "S-<left>" nil
        "S-<right>" nil
        "S-<up>" nil
        "S-<down>" nil
        :n "C-j" nil
        :n "C-k" nil
        :n "z a"   #'org-cycle
        :i [tab] #'completion-at-point
        :i "M-TAB" #'russ/org-roam-insert-file

        :localleader
        :n "r"     #'hydra-org-refile/body
        :n "i"     #'russ/org-roam-insert-node-level-0)

  (map! :map org-agenda-mode-map
        "C-k" nil)

  (map! :map org-agenda-keymap
        "C-k" nil))

(after! evil-org
  (map! :map evil-insert-state-map
        [tab] #'completion-at-point)

  (map! :map evil-org-agenda-mode-map
        "C-j" nil
        "C-k" nil

        :i "M-TAB" #'russ/org-roam-insert-file)

  (map! :map evil-normal-state-map
        :n "z a"   #'org-cycle
        "S-<left>" nil
        "S-<right>" nil
        "S-<up>" nil
        "S-<down>" nil
        "C-h" nil
        "C-l" nil
        "C-j" nil
        "C-k" nil
        "z w"   #'widen))

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      "p" #'org-agenda-priority)

(map! :after org-roam
      :map org-mode-map
      :localleader
      :prefix ("m" . "org-roam")
      ;; "f" #'org-roam-find-file
      "f" #'russ/org-roam-find-file
      "F" #'russ/org-roam-find-node-relevant
      ;; "g" #'org-roam-graph
      ;; "i" #'org-roam-insert
      "i" #'russ/org-roam-insert-file
      ;; "I" #'org-roam-insert-immediate
      "I" #'russ/org-roam-insert-node-relevant
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org projectile

;; (use-package! org-project-capture
;;   :after org-capture
;;   :config
;;   ;; (setq org-project-capture-backend
;;   ;;       (make-instance 'YOUR-CHOSEN-BACKEND))  ; Replace with your backend of choice
;;   ;; (setq org-project-capture-projects-file "~/org/projects.org")
;;   (setq org-projectile-per-project-filepath "todo.org")
;;   (org-project-capture-per-project)
;;   (push (org-projectile-project-todo-entry) org-capture-templates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org properties

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common urls

(setq russ/suggested-links
      '("https://danger.russmatney.com"
        "https://discord.gg/PQvfdApHFQ" ;; discord invite

        "https://ko-fi.com/russmatney"
        "https://mastodon.gamedev.place/@russmatney"
        "https://patreon.com/russmatney"
        "https://programming.dev/u/russmatney"

        ;; web games
        "https://games.russmatney.com"

        ;; github
        "https://github.com/russmatney"
        "https://github.com/russmatney/clawe"
        "https://github.com/russmatney/dino"
        "https://github.com/russmatney/dothop"
        "https://github.com/russmatney/org-blog"
        "https://github.com/russmatney/org-crud"
        "https://github.com/russmatney/russmatney"
        "https://github.com/russmatney/gg"
        "https://github.com/russmatney/bones"
        "https://github.com/russmatney/log.gd"
        "https://github.com/russmatney/clove"

        ;; blog
        "https://russmatney.com"
        ;; TODO these are broken
        "https://russmatney.com/devlogs/devlog_01_dino_so_far.html"
        "https://russmatney.com/devlogs/devlog_02_aseprite_scripting.html"
        "https://russmatney.com/#/devlogs/2024-01-08-zooming-in-on-dino"
        "https://russmatney.com/#/devlogs/2024-12-11-glossolalia-listen-prototyping"

        ;; itch
        "https://russmatney.itch.io"
        "https://russmatney.itch.io/dino"
        "https://russmatney.itch.io/dungeon-crawler"
        "https://russmatney.itch.io/runner"
        "https://russmatney.itch.io/blox"
        "https://russmatney.itch.io/thief-guard"
        "https://moonstorm-clerics.itch.io"
        "https://moonstorm-clerics.itch.io/overculted-prototype"
        "https://moonstorm-clerics.itch.io/terrorware-prototype"
        "https://moonstorm-clerics.itch.io/wicked-peak"
        "https://moonstorm-clerics.itch.io/rapid-eye-madness"
        "https://trensharo.itch.io/soul-commander"
        "https://cat-tale-games.itch.io"
        "https://cat-tale-games.itch.io/carving-with-care"

        ;; steam
        "https://store.steampowered.com/app/2589550/Dino"
        "https://store.steampowered.com/app/2779710/Dot_Hop"
        "https://store.steampowered.com/app/3248030/Rapid_Eye_Madness/"

        "https://trello.com/b/Uqzeh3aX/russ-game-dev-studio"
        "https://twitch.tv/russmatney"
        "https://youtube.com/@russmatney"
        "https://www.youtube.com/c/RussellMatney"
        "https://youtu.be/9cyAnNLGrZI" ;; 01 Dino Year One
        "https://youtube.com/playlist?list=PL2gEO25pE6dogWyUby_sCCTst0LUyCEUK" ;; devlog playlist
        ))

(after! org
  (org-link-set-parameters
   "https"
   :complete
   (lambda ()
     (completing-read "link: "
                      (cl-remove
                       "https:"
                       (delete-dups
                        (cl-concatenate 'list
                                        org-link--insert-history
                                        russ/suggested-links)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Roam

(setq org-roam-file-exclude-regexp
      ;; this is actually compared to a relative path, despite org-attach-id-dir not being one
      (list org-attach-id-dir
            "old/"
            "archive/"
            ;; (file-truename "~/todo/old/")
            ;; (expand-file-name "~/todo/old/")
            ))

(use-package! org-roam
  :config
  (require 'org-roam-dailies)

  ;; add recent dailies to org-agenda-files
  (setq recent-daily-dates
        (cl-loop for i from 0 below 60 collect
                 (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time i))))
        recent-dailies (cl-remove-if-not
                        (lambda (s)
                          (member (file-name-base s) recent-daily-dates))
                        (org-roam-dailies--list-files))

        org-agenda-files
        (cl-remove-duplicates
         (append org-agenda-files recent-dailies)
         :test #'string=)))

(after! org-roam
  ;; (setq org-roam-node-display-template)
  )

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target
           (file+head "~/todo/garden/${slug}.org"
                      "#+TITLE: ${title}
#+CREATED_AT: %<%Y%m%d:%H%M%S>
#+startup: content")
           :unnarrowed t)))

  ;; https://org-roam.discourse.group/t/v2-error-running-org-roam-dailies-find-today/1511/4
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>
#+created_at: %<%Y%m%d:%H%M%S>
#+startup: content")))))

(defun russ/org-roam--get-root-titles ()
  "Return all distinct titles and aliases in the Org-roam database."
  (mapcar #'car (org-roam-db-query [:select :distinct title :from nodes
                                    :where (= level 0)])))

(defun russ/org-roam-complete-everywhere ()
  "Modified version of `org-roam-complete-everywhere'"
  (when (and org-roam-completion-everywhere
             (thing-at-point 'word)
             (not (org-in-src-block-p))
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (russ/org-roam--get-root-titles)
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert "[[roam:" str "]]"))
            ;; Proceed with the next completion function if the returned titles
            ;; do not match. This allows the default Org capfs or custom capfs
            ;; of lower priority to run.
            :exclusive 'no))))

(after! org-roam
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              ;; #'org-roam-reflinks-section
              ;; #'org-roam-unlinked-references-section ;; note, can be slow!
              ))

  ;; overwrite completion functions here
  (setq org-roam-completion-functions
        (list #'org-roam-complete-link-at-point
              (cape-capf-case-fold #'russ/org-roam-complete-everywhere))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org roam ui

(use-package! org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save nil
        org-roam-ui-open-on-start nil
        ))

;;; sandbox ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
 (mapcar #'car (org-roam-db-query [:select :distinct title :from nodes
                                   :where (= level 0)]))

 (org-roam-db-query [:select * :from links])
 (org-roam-db-query [:select * :from tags])

 (cl-first
  (org-roam-node-read)
  )

 (org-roam-node-tags
  (cdr
   (cl-first
    (org-roam-node-read--completions
     (lambda (node)
       (= 1 (org-roam-node-level node)))
     ))))

 (org-roam-node-tags
  (org-roam-node-read
   nil
   (lambda (node)
     (org-roam-node-tags node))))

 (let ((node
        (cdr
         (cl-first
          (org-roam-node-read--completions
           (lambda (node)
             (= 0 (org-roam-node-level node)))
           )))))
   (org-roam-node-file-title node))

 (org-roam-node-read
  nil (lambda (node)
        (and
         (not (string-match-p
               "/old-nov-2020/\\|/old/\\|/drafts-journal/\\|/journal/\\|/archive/"
               (org-roam-node-file node)))
         (not (member "reviewed" (org-roam-node-tags node))))))

 (org-roam-node-read
  nil (lambda (node)
        (and
         (string-match-p
          "/old-nov-2020/\\|/old/\\|/drafts-journal/\\|/journal/\\|/archive/\\|/garden/"
          (org-roam-node-file node)))))
 )

(comment

 (org-roam-db-query
  [:select (funcall count)
   :from links])

 (org-roam-db-query
  [:select (funcall count)
   :from nodes])

 (org-roam-db-query
  [:select source
   :from links
   :group-by source
   :having (= (funcall count) 1)])

 (org-roam-db-query
  "SELECT source
  FROM links
  GROUP BY source
  HAVING COUNT(source) = 1;")


 (org-roam-db-query
  "SELECT COUNT(*) FROM (
  SELECT source
  FROM links
  GROUP BY source
  HAVING COUNT(source) = 1) as unique_ids;")

 )
