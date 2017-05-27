;;; init-org.el --- Org mode related config
;;; Commentary:
;;; Code:

(setq org-src-fontify-natively t)

;;(org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((lisp . t))
;;)

;; (defun my/tangle-dotfiles ()
;;   "If the current file is in '~/.dotfiles', the code blocks are tangled"
;;   (when (equal (file-name-directory (directory-file-name buffer-file-name))
;;                (concat (getenv "HOME") "/dotfiles/emacs.d/"))
;;     (org-babel-tangle)
;;     (message "%s tangled" buffer-file-name)))

;; (add-hook 'after-save-hook #'my/tangle-dotfiles)

(setq org-directory "~/Dropbox/todo/")
(setq org-mobile-inbox-for-pull "~/Dropbox/todo/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files '("~/Dropbox/todo"))

;;(add-hook 'after-init-hook 'org-mobile-pull)
;;(add-hook 'kill-emacs-hook 'org-mobile-push)

(setq org-default-notes-file (concat org-directory "/captured.org"))
     (define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo (any)" entry (file+headline "~/Dropbox/todo/todo.org" "Captured")
             "* TODO %?\n  Captured: %U\n  %a")
        ("r" "Read/Research" entry (file+headline "~/Dropbox/todo/todo.org" "Read")
             "* READ %?\n  Captured: %U\n  %a")
      )
)

(setq org-log-done 'time)

(setq org-agenda-files
      '("todo.org" "done.org" "read.org"))

(setq org-refile-use-outline-path 'file)


;; (eval-after-load 'org
;;   (progn
;;     (global-set-key (kbd "C-k") 'windmove-up)
;;     (global-set-key (kbd "C-j") 'windmove-down)))

(provide 'init-org)
;;; init-org.el ends here
