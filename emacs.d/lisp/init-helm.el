;;; init-helm.el --- Helm config
;;; Commentary:
;;;   - https://emacs.stackexchange.com/questions/13440/decomposing-helm-source-buffers-list-in-two-parts
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
    ("C-z" . helm-select-action)

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
          helm-M-x-fuzzy-match t
          helm-ff-newfile-prompt-p nil)

    (defclass my-helm-source-file-buffers-class (helm-source-buffers)
      ((candidates :initform
                   (lambda ()
                     (mapcar 'buffer-name
                             (cl-remove-if-not #'buffer-file-name (buffer-list)))))))

    (defclass my-helm-source-term-buffers-class (helm-source-buffers)
      ((candidates :initform
                   (lambda ()
                     (mapcar 'buffer-name
                             (cl-remove-if-not (lambda (buffer) (string-prefix-p "*term" (buffer-name buffer))) (buffer-list)))))))

    (defclass my-helm-source-other-buffers (helm-source-buffers)
      ((candidates :initform
                   (lambda ()
                     (mapcar 'buffer-name
                             (cl-remove-if #'buffer-file-name (buffer-list)))))))

    (setq
        my-helm-source-file-buffers-list (helm-make-source "File-Buffers" 'my-helm-source-file-buffers-class)
        my-helm-source-term-buffers-list (helm-make-source "Term Buffers" 'my-helm-source-term-buffers-class)
        my-helm-source-other-buffers-list (helm-make-source "Other" 'my-helm-source-other-buffers)
    )

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
          "~/Dropbox/todo/todo.org"
          "~/dotfiles/emacs.d/lisp/todo.org"
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


    (defvar rm/quick-config-files
      (helm-build-sync-source "Config Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/dotfiles/emacs.d/lisp/todo.org"
          "~/dotfiles/emacs.d/lisp/init-term.el"
          "~/dotfiles/emacs.d/lisp/init-evil.el"
          "~/dotfiles/emacs.d/lisp/init-helm.el"
          "~/dotfiles/emacs.d/lisp/init-extra.el"
          "~/dotfiles/emacs.d/lisp/init-org.el"
          "~/dotfiles/zshrc"
        )
      )
    )

    (use-package helm-ls-git)

    (setq helm-mini-default-sources '(my-helm-source-file-buffers-list
                                      my-helm-source-term-buffers-list
                                      my-helm-source-other-buffers-list
                                      helm-source-recentf
                                      helm-source-ls-git-status
                                      helm-source-projectile-projects
                                      helm-source-my-org-files
                                      helm-source-emacs-commands-history
                                      helm-source-buffer-not-found
                                      rm/quick-config-files
                                    )
    )

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
    )))

(use-package helm-ag
  :config
  (custom-set-variables
  '(helm-ag-ignore-patterns '(".*//doc//.*'"))
  )
)


(provide 'init-helm)
;;; init-helm.el ends here
