;;;  -*- lexical-binding: t; -*-


(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps nil

      ;; org-log-done 'note ;; <-- an interesting option
      org-log-done 'time

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
                                   (s-contains? "prompts" s)
                                   (s-contains? "reads" s)
                                   (s-contains? "watches" s)))
                                (file-expand-wildcards "~/Dropbox/todo/*.org"))
                               (file-expand-wildcards "~/Dropbox/notes/**/*.org"))

      org-todo-files (file-expand-wildcards "~/Dropbox/todo/*.org")
      org-journal-archive-files (file-expand-wildcards "~/Dropbox/todo/journal/*.org")
      org-dailies-files (file-expand-wildcards "~/Dropbox/notes/daily/*.org")

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
  (file-expand-wildcards "~/Dropbox/notes/**/*.org"))
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


(after! org-capture
  (setq org-capture-templates
        '(;; TODO use doct to write notes with hooks in sane way
          ;; ("d" "Daily [notes]" entry (file (org-roam-dailies-today)) "* %i%?")
          ("t" "Todo [journal]" entry (file "~/Dropbox/todo/journal.org") "* [ ] %i%?"))))

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
