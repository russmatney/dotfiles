;;;  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after markdown-mode
   (:map markdown-mode-map
     "M-p"    nil
     "M-n"    nil))

 (:after markdown-mode
   (:map evil-markdown-mode-map
     "M-p"    nil
     "M-n"    nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets
      '(("~/Dropbox/todo/todo.org" :maxlevel . 2)
        ("~/Dropbox/todo/specs.org" :maxlevel . 2))

      org-agenda-files (file-expand-wildcards "~/Dropbox/todo/*.org"))


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
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil
      "M-v"    #'evil-paste-after
      "M-RET"  #'org-insert-item
      "M-t"    #'org-set-tags-command
      "TAB"    #'+org/toggle-fold)

(map! :after evil-org
      :map evil-org-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil

      :map evil-normal-state-map
      "z w"    #'widen)

(map!
 (:after markdown
   (:map markdown-mode-map
     "M-n" nil
     "M-p" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org capture
(map!
 (:after org-capture
   (:map org-capture-mode-map
     [remap evil-save-and-close]          #'org-capture-finalize
     [remap evil-save-modified-and-close] #'org-capture-finalize
     [remap evil-quit]                    #'org-capture-kill)))

(after! org-capture
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                                 "* [ ] %i%?"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   (:desc "notes" :prefix "n"
     :desc "add story to clubhouse" :n "c" #'org-clubhouse-create-story)))
