;;; emux-helm.el --- Helm-based solutions to common term-based needs
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm list terminal buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass my-helm-source-term-buffers-class (helm-source-buffers)
  ((candidates :initform
               (lambda ()
                 (mapcar 'buffer-name
                         (cl-remove-if-not (lambda (buffer) (string-prefix-p "*term" (buffer-name buffer))) (buffer-list)))))))

(setq emux-helm-source-term-buffers-list (helm-make-source "Term Buffers" 'my-helm-source-term-buffers-class))

(defun emux-helm-term-buffers ()
  "List *term * buffers."
  (interactive)
  (helm :sources '(emux-helm-source-term-buffers-list)
  :buffer "*helm term buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm for shell history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-helm-shell-history ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "helm-shell-history"
                   :data (emux-shell-history)
                   :action 'emux-run-shell-command)
        :buffer "*helm shell history*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm for firing shell commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun emux-helm-shell-commands ()
  "Helm interface to fire shell commands in a local terminal session."
  (interactive)
  (helm :sources '(
                   emux-custom-shell-command-source
                   emux-git-chain-commands-source
                   emux-common-git-commands-source
                   emux-common-mix-command-source
                   )
        :buffer "*emux helm shell commands*"))

(defvar emux-common-mix-command-source
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

(defvar emux-common-git-commands-source
  (helm-build-in-buffer-source "Common git commands"
    :data '(
            "gst"
            "git diff --staged"
            "git add ."
            "git commit --amend --no-edit"
            )
    :action 'emux-run-shell-command))


(defvar emux-git-chain-commands-source
  (helm-build-in-buffer-source "Chainable git commands"
    :data '(
            "gco [branch-to-checkout]"
            )
    :action 'emux-helm-gco-branches))


(defvar emux-custom-shell-command-source
  (helm-build-in-buffer-source "Custom"
    :data '(
            "ENTER via prompt"
            )
    :action 'emux-run-shell-command-from-minibuffer-action))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-shell-history ()
  "List shell command history."
  (let ((history (shell-command-to-string "tac ~/.zsh_history | sed 's/^.*;//'")))
    (split-string history "\n")))

(defun emux-git-branches ()
  "List git branches by parsing shell output."
  (let ((branches
         (shell-command-to-string "git branch -a | tr -d '* ' | sed 's/^remotes\\/origin\\///' | sort | uniq")))
    (split-string branches "\n")))

(defun emux-term-checkout-branch (branch)
  "Fires `gco` BRANCH in a local term."
  (emux-run-shell-command (format "gco %s" branch) nil t))

(defun emux-helm-gco-branches (str)
  "Checkout a git branch with helm.
STR is ignored.
This is a convenience function for helm actions."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "git branches"
                   :data (emux-git-branches)
                   :action 'emux-term-checkout-branch)
        :buffer "*helm git branches*"))

(defun emux-run-shell-command-from-minibuffer-action (command)
  "Open the mini-buffer for command input.
COMMAND is ignored.
This is a convenience function for helm."
  (interactive)
  (emux-run-shell-command-from-minibuffer))


(provide 'emux-helm)
;;; emux-helm.el ends here
