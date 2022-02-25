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


      org-agenda-files (cl-remove-if
                        (lambda (s)
                          (or
                           (s-contains? "icebox" s)
                           (s-contains? "goals" s)
                           (s-contains? "ideas" s)
                           (s-contains? "urbint" s)
                           ;; (s-contains? "prompts" s)
                           (s-contains? "reads" s)
                           (s-contains? "watches" s)))
                        (file-expand-wildcards "~/Dropbox/todo/*.org"))

      org-garden-files (append (file-expand-wildcards "~/Dropbox/todo/garden/*.org")
                               (file-expand-wildcards "~/Dropbox/todo/garden/**/*.org")))

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
        "TAB"   #'+org/toggle-fold
        "M-o"   #'+org/insert-item-below
        "M-O"   #'+org/insert-item-above
        "S-<left>" nil
        "S-<right>" nil
        "S-<up>" nil
        "S-<down>" nil
        "C-j" nil
        :n "z a"   #'org-cycle

        :localleader
        :n "r"     #'hydra-org-refile/body)

  (map! :map org-agenda-mode-map
        "C-k" nil)

  (map! :map org-agenda-keymap
        "C-k" nil))

(after! evil-org
  (map! :map evil-org-agenda-mode-map
        "C-j" nil
        "C-k" nil)

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

;; TODO review these in light of v2
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain
           ;; (function org-roam--capture-get-point)
           "%?"
           :if-new
           (file+head "~/todo/garden/${slug}.org"
                      "#+TITLE: ${title}
#+CREATED_AT: %<%Y%m%d:%H%M%S>")
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
                      "#+title: %<%Y-%m-%d>"))))


  (setq
   recent-daily-dates (cl-loop for i from 0 below 14 collect
                               (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time i))))
   recent-dailies (cl-remove-if-not
                   (lambda (s)
                     (member (file-name-base s) recent-daily-dates))
                   (org-roam-dailies--list-files))

   org-agenda-files (append org-agenda-files recent-dailies)))

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "doom-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(setq +org-roam-open-buffer-on-find-file nil)


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

;; (use-package! org-pomodoro
;;   :config
;;   (setq org-pomodoro-length 35
;;         org-pomodoro-short-break-length 10))

;; (defun ruborcalor/org-pomodoro-time ()
;;   "Return the remaining pomodoro time"
;;   (if (org-pomodoro-active-p)
;;       (cl-case org-pomodoro-state
;;         (:pomodoro
;;          ;; TODO title case
;;          (format "%s: %d minutes" org-clock-heading (/ (org-pomodoro-remaining-seconds) 60)))
;;         (:short-break
;;          (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
;;         (:long-break
;;          (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
;;         (:overtime
;;          (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
;;     "No active pomo"))

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
              ))
  ;; disable by default because roam locks up emacs far too often
  ;; (org-roam-db-autosync-disable)
  )

;; https://orgmode-exocortex.com/2021/07/22/configure-org-roam-v2-to-update-database-only-when-idle/
;; update roam on idle, not on file-save
;; (with-eval-after-load "org-roam"
;;   ;; queue for files that will be updated in org-roam-db when emacs is idle
;;   (setq org-roam-db-update-queue (list))
;;   ;; save the original update function;
;;   (setq orig-update-file (symbol-function 'org-roam-db-update-file))
;;   ;; then redefine the db update function to add the filename to a queue
;;   (defun org-roam-db-update-file (&optional file-path)
;;     ;; do same logic as original to determine current file-path if not passed as arg
;;     (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
;;     (message "org-roam: scheduling update of %s" file-path)
;;     (if (not (memq file-path org-roam-db-update-queue))
;;         (push file-path org-roam-db-update-queue)))

;;   ;; this function will be called when emacs is idle for a few seconds
;;   (defun org-roam-db-idle-update-files ()
;;     ;; go through queued filenames one-by-one and update db
;;     ;; if we're not idle anymore, stop. will get rest of queue next idle.
;;     (while (and org-roam-db-update-queue (current-idle-time))
;;       ;; apply takes function var and list
;;       (apply orig-update-file (list (pop org-roam-db-update-queue)))))

;;   ;; we'll only start updating db if we've been idle for this many seconds
;;   (run-with-idle-timer 5 t #'org-roam-db-idle-update-files))
