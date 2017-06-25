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
    )

    (defun wc/git-branches ()
      (setq branches (shell-command-to-string "git branch -a | tr -d '* ' | sed 's/^remotes\\/origin\\///' | sort | uniq"))
      (split-string branches "\n"))

    (defun wc/shell-history ()
      (setq history (shell-command-to-string "tac ~/.zsh_history | sed 's/^.*;//'"))
      (split-string history "\n"))


    (defun wc/helm-shell-history ()
      "Reverse-I search using Helm."
      (interactive)
      (helm :sources (helm-build-in-buffer-source "helm-shell-history"
                       :data (wc/shell-history)
                       :action 'emux-run-shell-command)
            :buffer "*helm shell history*"))


    (defvar rm/common-mix-commands
      (helm-build-in-buffer-source "Common mix commands"
        :data '(
                "MIX_ENV=test iex -S mix"
                "iex -S mix"
                "mix test"
                "mix docs.dash"
                "mix deps.get"
                "mix compile --force"
                "mix credo --strict"
                "mix dialyzer"
                )
        :action 'emux-run-shell-command))


    (defvar rm/common-cli-commands
      (helm-build-in-buffer-source "Common cli commands"
        :data '(
                "git commit --amend --no-edit"
                "gst"
                "git diff --staged"
                )
        :action 'emux-run-shell-command))

    (defun rm/term-checkout-branch (branch)
      "Fires `gco` BRANCH in a local term."
      (emux-run-shell-command (format "gco %s" branch) nil t))

    (defun rm/helm-gco-branches (str)
      "Checkout a git branch with helm.
STR is ignored.
This is a convenience function for helm actions."
      (interactive)
      (helm :sources (helm-build-in-buffer-source "git branches"
                       :data (wc/git-branches)
                       :action 'rm/term-checkout-branch)
            :buffer "*helm git branches*"))


    (defvar rm/git-chain-commands
      (helm-build-in-buffer-source "Git branch commands"
        :data '(
                "gco [branch-to-checkout]"
                )
        :action 'rm/helm-gco-branches))

    (defvar rm/custom-command
      (helm-build-in-buffer-source "Custom"
        :data '(
                "ENTER via prompt"
                )
        :action 'rm/run-shell-command-from-minibuffer-action))

    (defun rm/helm-shell-commands ()
      "Helm interface to fire shell commands in a local terminal session."
      (interactive)
      (helm :sources '(
                       rm/custom-command
                       rm/git-chain-commands
                       rm/common-cli-commands
                       rm/common-mix-commands
                       ;; wc/discover-shell-commands
                       )
            :buffer "*helm mix commands*"))

    (defun rm/run-shell-command-from-minibuffer-action (command)
      "Open the mini-buffer for command input.
COMMAND is ignored.
This is a convenience function for helm."
      (interactive)
      (emux-run-shell-command-from-minibuffer))
  )

  (defun rm/helm-term-buffers ()
  "List *term * buffers"
  (interactive)
  (helm :sources my-helm-source-term-buffers-list)
    :buffer "*helm term buffers*")
  )

(use-package helm-ag
  :config
  (custom-set-variables
  '(helm-ag-ignore-patterns '(".*//doc//.*'"))
  )
)


(provide 'init-helm)
;;; init-helm.el ends here
