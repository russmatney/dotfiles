;;;  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after markdown-mode
   (:map markdown-mode-map
     "<backspace>" nil
     "<A-left>"    nil
     "<A-right>"   nil
     "A-<tab>"     #'markdown-cycle
     "<M-left>"    nil
     "<M-right>"   nil
     "M-<tab>"     #'markdown-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets
      '(("~/Dropbox/todo/todo.org" :maxlevel . 2)
        ("~/Dropbox/todo/specs.org" :maxlevel . 2)))


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
(map!
 (:after org
   (:map org-mode-map
     "M-j"    nil
     "M-k"    nil
     "M-h"    nil
     "M-l"    nil
     "A-j"    nil
     "A-k"    nil
     "A-h"    nil
     "A-l"    nil
     "M-v"    #'evil-paste-after
     "M-RET"  #'org-insert-item
     "M-t"    #'org-set-tags-command
     "TAB"    #'+org/toggle-fold)

   (:map evil-org-mode-map
     "M-h"    nil
     "M-l"    nil
     "A-h"    nil
     "A-l"    nil)
   (:map evil-normal-state-map
     "z w"    #'widen)))

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

(defun +russ/org-capture-hook ()
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                                 "* [ ] %i%?"))


        org-agenda-files '("~/Dropbox/todo/inbox.org"
                           "~/Dropbox/todo/todo.org"
                           "~/Dropbox/todo/urbint.org")))

(after! org-capture
  (+russ/org-capture-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! org-clubhouse)

(map!
 (:leader
   (:desc "notes" :prefix "n"
     :desc "add story to clubhouse" :n "c" #'org-clubhouse-create-story)))
