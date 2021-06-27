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
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
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
                               (file-expand-wildcards "~/Dropbox/todo/garden/**/*.org")))


(defun russ/reset-refile-targets ()
  (setq
   org-todo-files (file-expand-wildcards "~/Dropbox/todo/*.org")
   org-journal-archive-files (file-expand-wildcards "~/Dropbox/todo/journal/*.org")
   org-dailies-files (file-expand-wildcards "~/Dropbox/todo/daily/*.org")

   org-refile-targets
   '((org-journal-archive-files :maxlevel . 1)
     (nil :maxlevel . 9)
     (org-todo-files :maxlevel . 2)
     (org-dailies-files :maxlevel . 1))))

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


(setq +org-roam-open-buffer-on-find-file nil)



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

(defun my-buffer-file-name ()
  "Give the directory of (buffer-file-name), and replace the home path by '~'"
  (interactive)
  (if (buffer-file-name)
      (->>
          (file-name-directory
           (file-relative-name
            (buffer-file-name)
            (expand-file-name "~")))
        (s-replace "Dropbox/todo/" "")
        (s-replace "russmatney/" "")
        (s-replace-regexp "/$" ""))
    ""))

(setq org-agenda-prefix-format
      '((agenda  . "%(my-buffer-file-name)%i %-12:c%?-12t% s")
        (timeline  . "%(my-buffer-file-name)% s")
        (todo  . "%(my-buffer-file-name)%i %-12:c")
        (tags  . "%(my-buffer-file-name)%i %-12:c")
        (search . "%(my-buffer-file-name)%i %-12:c")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun russ/refile-to (file headline)
  "Move current headline to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))
    (switch-to-buffer (current-buffer))))

(defhydra hydra-org-refile (:exit t)
  ("r" org-refile "Org refile" :column "~/todo")
  ("t" (russ/refile-to "~/todo/projects.org" "Todos") "project.org/Todos")
  ("h" (russ/refile-to "~/todo/projects.org" "Hammock") "project.org/Hammock")
  ("i" (+org/refile-to-file nil "~/todo/icebox.org") "To icebox.org")

  ("g" russ/org-refile-to-existing-note "To existing note" :column "garden")
  ("c" russ/org-refile-to-new-note "Create new note")
  ("j" russ/org-refile-to-daily-note "To some daily note")
  ("w" russ/org-refile-to-workspace-note "To some workspace note")
  ("b" russ/org-refile-to-bucket-note "To a bucket note, i.e. ideas/writing accumulation files")

  ;; TODO refile to today's daily note, create if it doesn't exist
  )

(after! org
  (map! :map org-mode-map
        "M-v"   #'evil-paste-after
        "M-RET" #'org-insert-item
        "M-t"   #'org-set-tags-command
        "TAB"   #'+org/toggle-fold
        "M-o"   #'+org/insert-item-below
        "M-O"   #'+org/insert-item-above
        "S-<left>" nil
        "S-<right>" nil
        "S-<up>" nil
        "S-<down>" nil
        :n "z a"   #'org-cycle

        :localleader
        :n "r"     #'hydra-org-refile/body))

(after! evil-org
  (map! :map evil-normal-state-map
        :n "z a"   #'org-cycle
        "S-<left>" nil
        "S-<right>" nil
        "S-<up>" nil
        "S-<down>" nil
        "z w"   #'widen))

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      "p" #'org-agenda-priority)

(after! org-roam
  (map! :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "r" #'russ/org-refile-to-existing-note
        "R" #'russ/org-refile-to-new-note
        "j" #'russ/org-refile-to-daily-note
        "w" #'russ/org-refile-to-workspace-note))

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

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "garden/${slug}"
           :head
           "#+TITLE: ${title}
#+ID: %(shell-command-to-string \"uuidgen\")#+CREATED_AT: %<%Y%m%d:%H%M%S>"
           :unnarrowed t))

        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "garden/websites/${slug}"
           :head "#+TITLE: ${title}
#+CREATED_AT: %<%Y%m%d:%H%M%S>
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t))

        ;; org-roam-dailies-capture-templates
        ))

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
 :prefix "n"
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
