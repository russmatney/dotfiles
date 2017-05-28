;;; init-helm.el --- Helm config
;;; Commentary:
;;; Code:

(use-package helm
  :bind (
    ("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
    ("C-x C-b" . helm-buffers-list)

    :map helm-map
    ([backtab] . helm-previous-source)
    ([tab] . helm-next-source)
    ("C-j" . helm-next-line)
    ("C-k" . helm-previous-line)
    ("C-?" . describe-key)
    ([escape] . helm-keyboard-quit)

    :map helm-find-files-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)

    :map helm-read-file-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)
  )

  :init (helm-mode 1)

  :config
  (progn
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t)

    (defvar helm-source-emacs-commands
      (helm-build-sync-source "Emacs commands"
        :candidates (lambda ()
                      (let ((cmds))
                        (mapatoms
                        (lambda (elt) (when (commandp elt) (push elt cmds))))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "A simple helm source for Emacs commands.")

    (defvar helm-source-emacs-commands-history
      (helm-build-sync-source "Emacs commands history"
        :candidates (lambda ()
                      (let ((cmds))
                        (dolist (elem extended-command-history)
                          (push (intern elem) cmds))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "Emacs commands history")

    (defvar helm-source-my-org-files
      (helm-build-sync-source "Org Files"
        :action 'helm-type-file-actions
        :candidates '(
          ;; "~/dotfiles/emacs.d/init.org"
          "~/Dropbox/todo/todo.org"
          "~/Dropbox/todo/notes.org"
          "~/Dropbox/Writing/writing-may-2017.org"
          "~/Dropbox/Writing/triage.org"
          "~/Dropbox/todo/blog.org"
          "~/Dropbox/todo/storyx.org"
          "~/Dropbox/todo/opensource.org"
          "~/Dropbox/todo/urbint.org"
        )
      )
    )

    (use-package helm-ls-git)

    (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-ls-git-status
                                      helm-source-projectile-projects
                                      helm-source-my-org-files
                                      helm-source-emacs-commands-history
                                      helm-source-emacs-commands
                                      helm-source-buffer-not-found))

    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on)

        (setq helm-projectile-sources-list
          '(helm-source-projectile-buffers-list
            helm-source-projectile-recentf-list
            helm-source-projectile-files-list
            helm-source-projectile-projects
            helm-source-my-org-files
           )
        )
      )
    )

    (setq helm-boring-buffer-regexp-list
      (quote ( "\\Minibuf.+\\*"
               "\\` "
               ;; "\\*.+\\*"
             )
      )
    )

    ;; TODO: doesn't work for un'opened' files, only existing buffers
    (defun helm-buffer-switch-to-new-window (_candidate)
      "Display buffers in new windows."
      ;; Select the bottom right window
      (require 'winner)
      (select-window (car (last (winner-sorted-window-list))))
      ;; Display buffers in new windows
      (dolist (buf (helm-marked-candidates))
        (select-window (split-window-right))
        (switch-to-buffer buf))
      ;; Adjust size of windows
      (balance-windows))

    (add-to-list 'helm-type-buffer-actions
                '("Display buffer(s) in new window(s) `M-o'" .
                  helm-buffer-switch-new-window) 'append)

    (defun helm-buffer-switch-new-window ()
      (interactive)
      (with-helm-alive-p
        (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window)))

    (define-key helm-map (kbd "M-o") #'helm-buffer-switch-new-window)

  )
)

(use-package helm-ag
  :config
  (custom-set-variables
  '(helm-ag-ignore-patterns '(".*//doc//.*'"))
  )
)


(provide 'init-helm)
;;; init-helm.el ends here
