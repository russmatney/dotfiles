;;;  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :after markdown-mode
      :map markdown-mode-map
      "M-p"    nil
      "M-n"    nil

      :map evil-markdown-mode-map
      "M-p"    nil
      "M-n"    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps nil
      org-agenda-files (file-expand-wildcards "~/Dropbox/todo/*.org")

      org-journal-archive-files (file-expand-wildcards "~/Dropbox/todo/journal/*.org")
      org-dailies-files (file-expand-wildcards "~/Dropbox/notes/daily/*.org")

      org-refile-targets
      '((org-journal-archive-files :maxlevel . 1)
        (nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 2)
        (org-dailies-files :maxlevel . 2)
        ))

(advice-add 'org-archive-subtree
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-refile
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org
(map! :after org
      :map org-mode-map
      "M-j"    nil
      "M-k"    nil
      "M-h"    nil
      "M-l"    nil
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil
      "M-v"    #'evil-paste-after
      "M-RET"  #'org-insert-item
      "M-t"    #'org-set-tags-command
      "TAB"    #'+org/toggle-fold

      :map evil-org-agenda-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil)

(map! :after evil-org
      :map evil-org-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil

      :map evil-normal-state-map
      "z w"    #'widen

      :map evil-org-agenda-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil)


(map! :after markdown
      :map markdown-mode-map
      "M-n" nil
      "M-p" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; org capture
(map! :after org-capture
      :map org-capture-mode-map
      [remap evil-save-and-close]          #'org-capture-finalize
      [remap evil-save-modified-and-close] #'org-capture-finalize
      [remap evil-quit]                    #'org-capture-kill)

(after! org-capture
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
           "* [ ] %i%?"))))

(after! org-roam
  ;; https://github.com/org-roam/org-roam/issues/217
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
           (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    "No active pomo"))
