;;;  -*- lexical-binding: t; -*-

(require 'doct)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(use-package! org-rich-yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

(setq org-todo-keywords
      '((sequence
         "[ ](T)"                       ; A task that needs doing
         "[-](S)"                       ; Task is in progress
         "[?](W)"                       ; Task is being held up or paused
         "|"
         "[X](D)"      ; Task was completed
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
         ;; "KILL(k)"
         ;;     ;; Task was cancelled, aborted or is no longer applicable
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
      '(("Effort_ALL" . "1 2 3 5 8 13 21 34 55")
        )
      )

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      org-outline-path-complete-in-steps nil

      ;; startup folded by default (overwritable per file)
      org-startup-folded t

      ;; org-log-done 'note ;; <-- an interesting option
      org-log-done 'time
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
      org-agenda-skip-scheduled-if-deadline-is-shown t


      org-garden-files (append (file-expand-wildcards "~/Dropbox/todo/garden/*.org")
                               (file-expand-wildcards "~/Dropbox/todo/garden/**/*.org")))

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

(setq org-trello-files '("~/Dropbox/todo/studio_trello.org")
      org-trello-add-tags nil)

(setq org-roam-file-exclude-regexp
      ;; this is actually compared to a relative path, despite org-attach-id-dir not being one
      (list org-attach-id-dir
            "old/"
            "archive/"
            ;; (file-truename "~/todo/old/")
            ;; (expand-file-name "~/todo/old/")
            ))

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

(comment
 (append
  (cl-remove-if
   (lambda (s)
     (or
      (s-contains? "icebox" s)
      (s-contains? "goals" s)
      (s-contains? "ideas" s)
      (s-contains? "prompts" s)
      (s-contains? "reads" s)
      (s-contains? "watches" s)))
   (file-expand-wildcards "~/Dropbox/todo/*.org"))
  (file-expand-wildcards "~/Dropbox/todo/garden/**/*.org"))
 )



(advice-add 'org-archive-subtree
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-refile
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-agenda-redo :after 'org-save-all-org-buffers)


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
;; Org refile helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (org-roam-dailies-capture-tomorrow n t)
      ;; TODO write the file?
      )
    (russ/refile-to file-s "new")))

(comment
 (format-time-string "%Y-%m-%d.org" (current-time))
 (format-time-string "%Y-%m-%d.org" (time-add 86400 (current-time)))
 (format-time-string "%Y-%m-%d.org" (time-add (* (- 1) 86400) (current-time))))

(defhydra hydra-org-refile-daily (:exit t)
  ;; TODO refile to today's daily note, create if it doesn't exist
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
;; Org bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        "C-j" nil
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

(use-package! org-roam
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        ;; "b" #'org-roam-switch-to-buffer
        ;; "f" #'org-roam-find-file
        "f" #'russ/org-roam-find-file
        "F" #'russ/org-roam-find-node-relevant
        ;; "g" #'org-roam-graph
        ;; "i" #'org-roam-insert
        "i" #'russ/org-roam-insert-file
        ;; "I" #'org-roam-insert-immediate
        "I" #'russ/org-roam-insert-node-relevant
        ;; "m" #'org-roam
        ;; "t" #'org-roam-tag-add
        ;; "T" #'org-roam-tag-delete
        ;; (:prefix ("d" . "by date")
        ;;  :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
        ;;  :desc "Find date"          "d" #'org-roam-dailies-find-date
        ;;  :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
        ;;  :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
        ;;  :desc "Capture today"      "n" #'org-roam-dailies-capture-today
        ;;  :desc "Find today"         "t" #'org-roam-dailies-find-today
        ;;  :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
        ;;  :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
        ;;  :desc "Find directory"     "." #'org-roam-dailies-find-directory)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org 'turn-on-auto-fill)

(setq russ/suggested-links
      '("https://github.com/russmatney"
        "https://github.com/russmatney/org-blog"
        "https://github.com/russmatney/org-crud"
        "https://github.com/russmatney/clawe"
        "https://github.com/russmatney/dino"
        "https://github.com/russmatney/dothop"
        "https://github.com/russmatney/russmatney"
        "https://russmatney.itch.io"
        "https://russmatney.itch.io/dino"
        "https://russmatney.itch.io/runner"
        "https://russmatney.itch.io/dungeon-crawler"
        "https://patreon.com/russmatney"
        "https://danger.russmatney.com"
        "https://russmatney.com"
        "https://twitch.tv/russmatney"
        "https://youtube.com/@russmatney"
        "https://mastodon.gamedev.place/@russmatney"
        "https://programming.dev/u/russmatney"
        "https://store.steampowered.com/app/2589550/Dino"
        "https://store.steampowered.com/app/2779710/Dot_Hop"
        "https://discord.gg/xZHWtGfAvF" ;; discord invite
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
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org-capture
  (map! :map org-capture-mode-map
        [remap evil-save-and-close]          #'org-capture-finalize
        [remap evil-save-modified-and-close] #'org-capture-finalize
        [remap evil-quit]                    #'org-capture-kill))


(comment
 (setq org-capture-templates
       '(("t" "Todo [journal]" entry (file "~/todo/journal.org") "* [ ] %i%?")
         ("r" "Prompt" entry (file "~/todo/prompts.org") "* [ ] %i%?")
         ("d" "Garden Daily" entry #'org-roam-dailies-capture-today nil))))

(after! org-capture
  (setq org-capture-templates
        (doct '(("Todo [journal]"
                 :keys "t"
                 :file "~/todo/journal.org"
                 :template ("* [ ] %i%?"))
                ("Prompt"
                 :keys "r"
                 :file "~/todo/prompts.org"
                 :template ("* [ ] %i%?"))
                ("Garden Daily"
                 :keys "d"
                 :function org-roam-dailies-capture-today)))))

(use-package! org-roam
  :config
  (require 'org-roam-dailies)
  (setq recent-daily-dates (cl-loop for i from 0 below 60 collect
                                    (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time i))))
        recent-dailies (cl-remove-if-not
                        (lambda (s)
                          (member (file-name-base s) recent-daily-dates))
                        (org-roam-dailies--list-files))

        org-agenda-files
        (cl-remove-duplicates
         (append org-agenda-files recent-dailies)
         :test #'string=)))

;; TODO review these in light of v2
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain
           ;; (function org-roam--capture-get-point)
           "%?"
           :if-new
           (file+head "~/todo/garden/${slug}.org"
                      "#+TITLE: ${title}
#+CREATED_AT: %<%Y%m%d:%H%M%S>
#+startup: content")
           :unnarrowed t))

        org-roam-capture-ref-templates
        '(("r" "ref" plain
           ;; (function org-roam-capture--get-point)
           "%?"
           :if-new (file+head "~/todo/garden/websites/${slug}.org"
                              "#+TITLE: ${title}
#+CREATED_AT: %<%Y%m%d:%H%M%S>
#+ROAM_KEY: ${ref}
- source :: ${ref}")
           :unnarrowed t)))

  ;; https://org-roam.discourse.group/t/v2-error-running-org-roam-dailies-find-today/1511/4
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?" :if-new
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>
#+created_at: %<%Y%m%d:%H%M%S>
#+startup: content")))))

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "doom-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(setq +org-roam-open-buffer-on-find-file nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-projectile
  :after org-capture
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        ;; org-agenda-files (append org-agenda-files
        ;;                          ;; TODO filter for existing
        ;;                          ;; and maybe for contains /russmatney/teknql/
        ;;                          (org-projectile-todo-files))
        )

  (push (org-projectile-project-todo-entry) org-capture-templates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; org roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org-roam
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              ;; #'org-roam-unlinked-references-section ;; note, can be slow!
              )))

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

;; overwrite completion functions here
(setq org-roam-completion-functions
      (list #'org-roam-complete-link-at-point
            (cape-capf-case-fold #'russ/org-roam-complete-everywhere)))

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
         (not (member "reviewed" (org-roam-node-tags node)))))))
