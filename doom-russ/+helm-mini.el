;;; private/russ/+helm-mini.el -*- lexical-binding: t; -*-

(after! helm
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
        my-helm-source-other-buffers-list (helm-make-source "Other" 'my-helm-source-other-buffers))

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
          "~/Dropbox/todo/inbox.org"
          "~/Dropbox/todo/tickler.org"
          "~/Dropbox/todo/gtd.org"
          "~/Dropbox/todo/someday.org"
          "~/Dropbox/Writing/writing-may-2017.org"
          "~/Dropbox/Writing/triage.org"
          "~/dotfiles/emacs.d/lisp/wip.org"
          )))

    (defvar rm/quick-config-files
      (helm-build-sync-source "Config Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/dotfiles/emacs.d/lisp/wip.org"
          "~/dotfiles/emacs.d/lisp/init-term.el"
          "~/dotfiles/emacs.d/lisp/init-evil.el"
          "~/dotfiles/emacs.d/lisp/init-helm.el"
          "~/dotfiles/emacs.d/lisp/init-extra.el"
          "~/dotfiles/emacs.d/lisp/init-org.el"
          "~/dotfiles/emacs.d/lisp/init-themes.el"
          "~/dotfiles/emacs.d/lisp/init-home.el"
          "~/dotfiles/emacs.d/lisp/init-settings.el"
          "~/dotfiles/emacs.d/lisp/init-window-management.el"
          "~/dotfiles/zshrc"

          "~/dotfiles/emacs.d/lisp/emux.el"
          "~/dotfiles/emacs.d/lisp/emux-pager.el"
          "~/dotfiles/emacs.d/lisp/emux-helm.el"

          "~/dotfiles/russ-doom-module/+bindings.el"
          "~/dotfiles/russ-doom-module/+commands.el"
          "~/dotfiles/russ-doom-module/config.el"
          "~/dotfiles/russ-doom-module/init.el"
          )))

  (setq helm-mini-default-sources '(my-helm-source-file-buffers-list
                                    my-helm-source-term-buffers-list
                                    my-helm-source-other-buffers-list
                                    helm-source-recentf
                                    helm-source-projectile-projects
                                    helm-source-my-org-files
                                    helm-source-emacs-commands-history
                                    helm-source-buffer-not-found
                                    rm/quick-config-files
                                    )))
