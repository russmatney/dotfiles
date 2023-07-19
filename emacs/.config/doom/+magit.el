;;; ../dotfiles/emacs/.doom.d/+magit.el -*- lexical-binding: t; -*-


;; Git

(defhydra git-hydra (:exit t)
  ("s" magit-status "Magit status" :column "Magit")
  ("m" magit "Magit")
  ("b" magit-blame "Magit blame")
  ("l" magit-log "Magit log")
  ("t" git-timemachine-toggle "Git Time Machine toggle" :column "Time Machine")
  ("r" git-gutter:revert-hunk "Git Gutter revert hunk" :column "Gutter")
  ("a" git-gutter:stage-hunk "Git Gutter stage hunk"))

(map!
 (:leader
  (:desc "git" :n "g" #'git-hydra/body)))

;; allow moving left/right in magit buffers
(use-package! magit
  :config
  (map!
   (:map magit-mode-map
    "l" nil
    "h" nil
    :n "C-k" nil
    :n "C-j" nil)
   (:map magit-diff-mode-map
    "C-k" nil
    "C-j" nil)
   (:map code-review-mode-map
    :n "C-k" nil
    :n "C-j" nil)
   ;; TODO this doesn't belong here!
   (:map org-roam-mode-map
    :n "C-k" nil
    :n "C-j" nil)))

(map!
 ;; git-gutter
 :n  "]d" #'git-gutter:next-hunk
 :n  "[d" #'git-gutter:previous-hunk

 ;; evil-magit
 (:after evil-magit
  :map (magit-status-mode-map magit-revision-mode-map)
  :n "C-j" nil
  :n "C-k" nil)

 ;; magit
 :n  "gm" #'magit
 :n  "gl" #'magit-log-head
 :n  "gt" #'git-timemachine

 ;; git-timemachine
 (:after git-timemachine
  (:map git-timemachine-mode-map
   :nv "p" #'git-timemachine-show-previous-revision
   :nv "C-k" #'git-timemachine-show-previous-revision
   :nv "n" #'git-timemachine-show-next-revision
   :nv "C-j" #'git-timemachine-show-previous-revision
   :nv "g" #'git-timemachine-show-nth-revision
   :nv "c" #'git-timemachine-show-commit
   :nv "q" #'git-timemachine-quit
   :nv "w" #'git-timemachine-kill-abbreviated-revision
   :nv "W" #'git-timemachine-kill-revision
   :nv "b" #'git-timemachine-blame)))

;; Magit

;; (add-hook 'magit-mode-hook 'magit-todos-mode)

(use-package! magit
  :config
  (setq magit-repository-directories
        '(("~/dotfiles" . 0)
          ("~/russmatney" . 1))

        magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident nil)
          ("Dirty" 3 magit-repolist-column-dirty (:right-align t))
          ("Unpulled" 3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("Unpushed" 3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path" 99 magit-repolist-column-path nil)))

  ;; duplicate in most situations, but idc, it's annoys me when it's hidden
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-recent-commits nil t))

;; (use-package! magit-todos
;;   :config
;;   (setq magit-todos-rg-extra-args '("--hidden")
;;         magit-todos-branch-list 'branch
;;         magit-todos-branch-list-merge-base-ref "origin/main"
;;         ))


;; (use-package! magit-org-todos
;;   :config
;;   (magit-org-todos-autoinsert))

(use-package! forge
  ;; :config
  ;; (magit-add-section-hook 'magit-status-sections-hook
  ;;                         'forge-insert-authored-pullreqs
  ;;                         'forge-insert-pullreqs nil)
  ;; (magit-add-section-hook 'magit-status-sections-hook
  ;;                         'forge-insert-requested-reviews
  ;;                         'forge-insert-pullreqs nil)
  ;; (transient-append-suffix 'forge-dispatch '(0 -1)
  ;;   ["Misc"
  ;;    ("y" "yank" forge-copy-url-at-point-as-kill)])
  ;; (transient-append-suffix 'forge-dispatch '(0 -1)
  ;;   ["Edit"
  ;;    ("e a" "assignees" forge-edit-topic-assignees)
  ;;    ("e l" "labels" forge-edit-topic-labels)
  ;;    ("e r" "review requests" forge-edit-topic-review-requests)])
  )


(use-package! browse-at-remote)
