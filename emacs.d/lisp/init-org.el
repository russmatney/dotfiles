;;; init-org.el --- Org mode related config
;;; Commentary:
;;; Code:

(setq org-src-fontify-natively t)

(setq org-directory "~/Dropbox/todo/")

(setq org-default-notes-file (concat org-directory "/captured.org"))
     (define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/todo/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-agenda-files '("~/Dropbox/todo/inbox.org"
                         "~/Dropbox/todo/gtd.org"
                         "~/Dropbox/todo/tickler.org"))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; (setq org-refile-use-outline-path 'file)
(setq org-refile-targets '(("~/Dropbox/todo/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/todo/someday.org" :level . 1)
                           ("~/Dropbox/todo/tickler.org" :maxlevel . 2)))

;; (setq org-agenda-custom-commands
;;       '(("o" "At the office" tags-todo "@office"
;;          ((org-agenda-overriding-header "Office")))))

(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; bindings to fix in org-mode
(defun rm/org-mode-hook ()
  ;; restore vertical window movement
  (define-key org-mode-map (kbd "C-k") nil)
  (define-key org-mode-map (kbd "C-j") nil)

  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
  )
)

(add-hook 'org-mode-hook 'rm/org-mode-hook)


(provide 'init-org)
;;; init-org.el ends here
