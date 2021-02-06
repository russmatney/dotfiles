;;;  -*- lexical-binding: t; -*-

(require 'doct)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

(setq org-todo-keywords
      '((sequence
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
         )    ;; Task was cancelled, aborted or is no longer applicable
        (sequence
         "[ ](T)"                       ; A task that needs doing
         "[-](S)"                       ; Task is in progress
         "[?](W)"                       ; Task is being held up or paused
         "|"
         "[X](D)"      ; Task was completed
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
        ("HOLD" . +org-todo-onhold)))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps nil

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

      org-agenda-files (append (cl-remove-if
                                (lambda (s)
                                  (or
                                   (s-contains? "icebox" s)
                                   (s-contains? "goals" s)
                                   (s-contains? "ideas" s)
                                   ;; (s-contains? "prompts" s)
                                   (s-contains? "reads" s)
                                   (s-contains? "watches" s)))
                                (file-expand-wildcards "~/Dropbox/todo/*.org"))
                               )

      org-garden-files (append (file-expand-wildcards "~/Dropbox/todo/garden/*.org")
                               (file-expand-wildcards "~/Dropbox/todo/garden/**/*.org"))

      org-todo-files (file-expand-wildcards "~/Dropbox/todo/*.org")
      org-journal-archive-files (file-expand-wildcards "~/Dropbox/todo/journal/*.org")
      org-dailies-files (file-expand-wildcards "~/Dropbox/todo/garden/daily/*.org")

      org-refile-targets
      '((org-journal-archive-files :maxlevel . 1)
        (nil :maxlevel . 9)
        (org-todo-files :maxlevel . 2)
        (org-dailies-files :maxlevel . 2)))

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
      '(
        ;; TODO note that this misses items scheduled beyond the current agenda
        ("n" "Agenda and unscheduled TODOs"
         ((agenda "") (alltodo "" ((org-agenda-todo-ignore-with-date t)))))
        ("i" "Icebox"
         ((alltodo "" ((org-agenda-files (file-expand-wildcards "~/todo/icebox.org"))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org
  (map! :map org-mode-map
        "M-v"   #'evil-paste-after
        "M-RET" #'org-insert-item
        "M-t"   #'org-set-tags-command
        "TAB"   #'+org/toggle-fold
        "M-o"   #'+org/insert-item-below
        "M-O"   #'+org/insert-item-above
        :n "z a"   #'org-cycle))

(after! evil-org
  (map! :map evil-normal-state-map
        :n "z a"   #'org-cycle
        "z w"   #'widen))

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      "p" #'org-agenda-priority)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org-capture
  (map! :map org-capture-mode-map
        [remap evil-save-and-close]          #'org-capture-finalize
        [remap evil-save-modified-and-close] #'org-capture-finalize
        [remap evil-quit]                    #'org-capture-kill))


(comment
 '(("t" "Todo [journal]" entry (file "~/todo/journal.org") "* [ ] %i%?")
   ("r" "Prompt" entry (file "~/todo/prompts.org") "* [ ] %i%?")
   ("d" "Garden Daily" entry #'org-roam-dailies-capture-today nil))


 )

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

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head
           "#+TITLE: ${title}
#+ID: %(shell-command-to-string \"uuidgen\")"
           :unnarrowed t))

        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t))))

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "doom-capture" (frame-parameter nil 'name))
      (delete-other-windows)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :leader
   :desc "notes" :prefix "n"
   :desc "add story to clubhouse" :n "c" #'org-clubhouse-create-story)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org pomodoro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-pomodoro
  :config
  (setq org-pomodoro-length 35
        org-pomodoro-short-break-length 10))

(defun ruborcalor/org-pomodoro-time ()
  "Return the remaining pomodoro time"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro
         ;; TODO title case
         (format "%s: %d minutes" org-clock-heading (/ (org-pomodoro-remaining-seconds) 60)))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    "No active pomo"))

(defun russ/current-clock-string ()
  (if org-clock-current-task
      (substring-no-properties org-clock-current-task)))

(comment
 (org-element-property :raw-value org-clock-current-task)
 (org-element-type org-clock-current-task)
 (org-element--get-node-properties)
 (org-clock-get-clock-string)

 (text-properties-at 0 org-clock-current-task)

 (org-with-clock
     (org-entry-get nil "ITEM")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-projectile
  :after org-capture
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files
                                 ;; TODO filter for existing
                                 ;; and maybe for contains /russmatney/teknql/
                                 (org-projectile-todo-files)))

  (push (org-projectile-project-todo-entry) org-capture-templates))
