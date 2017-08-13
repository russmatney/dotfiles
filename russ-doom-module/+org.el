;;; private/russ/+org.el -*- lexical-binding: t; -*-


(setq org-directory (expand-file-name "~/Dropbox/todo/"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/todo/tickler.org" "Tickler")
                               "* %i%? \n %U")))


(setq org-agenda-files '("~/Dropbox/todo/inbox.org"
                         "~/Dropbox/todo/gtd.org"
                         "~/Dropbox/todo/tickler.org"))

(setq org-refile-targets '(("~/Dropbox/todo/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/todo/someday.org" :level . 1)
                           ("~/Dropbox/todo/tickler.org" :maxlevel . 2)))

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))
