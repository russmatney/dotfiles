;;; init-term.el --- Configuration for
;;; Commentary:
;;;   much thanks to:
;;;     - http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;;;     - https://emacs.stackexchange.com/questions/328/how-to-override-keybindings-for-term
;;;     - http://oremacs.com/2015/01/01/three-ansi-term-tips/
;;; Code:

;;; git diff and glp are too tall
;;; fzf -> use helm for c-r, c-t, c-y

;;; TODO show always-terminal across bottom in projectile project root
;;; TODO toggle terminal full-screen, partial-screen, off-screen

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer)
        )
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-zsh)
  (interactive (ansi-term "/bin/zsh")))
(ad-activate 'ansi-term)

(defun rm/projectile-run-term ()
  "Invoke `term' in the project's root."
  (interactive)
  (let* ((term (concat "term " (projectile-project-name)))
         (buffer (concat "*" term "*")))
    (unless (get-buffer buffer)
      (require 'term)
      (let ((program "/bin/zsh"))
        (projectile-with-default-dir (projectile-project-root)
          (set-buffer (make-term term program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer)))

(defun rm/term-exec-hook ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
)

;; force term-mode to expose the passed global binding
(defun expose-global-binding-in-term (binding)
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))

(defun rm/evil-open-at-bottom ()
  (interactive)
  (end-of-buffer)
  (evil-insert-state 1)
)

(eval-after-load "term"
  '(progn
     ;; ensure that scrolling doesn't break on output
     (setq term-scroll-to-bottom-on-output t)
  )
)


(defun rm/term-mode-hook ()
  (goto-address-mode)
  (linum-mode -1)

  (setq window-max-chars-per-line 1000)
  (setq term-scroll-show-maximum-output t)


  ;; expose for Ctrl-{h,j,k,l} window movement
  (expose-global-binding-in-term (kbd "C-l"))
  (expose-global-binding-in-term (kbd "C-h"))
  (expose-global-binding-in-term (kbd "C-j"))
  (expose-global-binding-in-term (kbd "C-k"))

  ;; keep M-x
  (expose-global-binding-in-term (kbd "M-x"))
  (expose-global-binding-in-term (kbd "M-:"))

  ;; ensure these are unset in term
  (evil-define-key 'insert term-raw-map
    (kbd "C-k") nil
    (kbd "C-p") nil
    (kbd "C-n") nil
    (kbd "C-r") nil
    (kbd "C-t") nil
    (kbd "C-e") nil
    (kbd "C-a") nil
    (kbd "C-c") 'term-interrupt-subjob
  ;;   (kbd "ESC") 'term-pager-discard
  )
  (evil-define-key 'normal term-raw-map
    (kbd "C-c") 'term-interrupt-subjob
    (kbd "i") 'rm/evil-open-at-bottom
  )

  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "M-j") 'wc/helm-autojump)
  (define-key term-raw-map (kbd "M-g") 'wc/helm-git-branches)
  (define-key term-raw-map (kbd "M-m") 'rm/helm-mix-commands)
  (define-key term-raw-map (kbd "s-v") 'term-paste)
)

(add-hook 'term-exec-hook 'rm/term-exec-hook)
(add-hook 'term-mode-hook 'rm/term-mode-hook)
(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAIDED from @wpcarro, TODO formalize
;;;; https://github.com/wpcarro/pc_settings/commit/aab701e10e52afec790b069a5e14c961c6f32307
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wc/shell-history ()
  ;; TODO fix history command (w/ shell script?)
  (setq history (shell-command-to-string "tac ~/.zsh_history | sed 's/^.*;//'"))
  (split-string history "\n"))

(defun wc/git-branches ()
  (setq branches (shell-command-to-string "git branch -a | tr -d '* ' | sed 's/^remotes\\/origin\\///' | sort | uniq"))
  (split-string branches "\n"))

(defun wc/helm-git-branches ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "git branches"
                 :data (wc/git-branches)
                 :action 'wc/handle-branch)
      :buffer "*helm git branches*"))

(defun wc/autojump-directories ()
  (setq directories (shell-command-to-string "j -s | awk '{ if($2 ~ /^\\// && $1 != \"data:\") print;}' | sort -rn | head -n 100 | awk '{print $2}'"))
  (split-string directories "\n"))

(defun wc/helm-autojump ()
  "Helm interface to autojump."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "helm-autojump"
                 :data (wc/autojump-directories)
                 :action (lambda (path) (wc/exec-cmd (format "cd %s" path))))
      :buffer "*helm git branches*"))

(defun wc/handle-branch (branch)
  (setq action "git diff")
  (term-send-raw-string (format "%s %s" action branch)))

(defun wc/helm-shell-history ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "helm-shell-history"
                 :data (wc/shell-history)
                 :action 'wc/exec-cmd)
      :buffer "*helm shell history*"))

(defun wc/exec-cmd (cmd)
  (term-send-raw-string (format "%s\n" cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end raid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/send-projectile-buffer-raw-string (command)
  "Invoke `command' root ansi-term session"
  (interactive)
  (let* ((term (concat "term " (projectile-project-name)))
         (buffer (concat "*" term "*")))
    (unless (get-buffer buffer)
      (let ((program "/bin/zsh"))
        (projectile-with-default-dir (projectile-project-root)
          (set-buffer (make-term term program))
          (term-mode)
          (term-char-mode)
        )
      )
    )
    (switch-to-buffer buffer)
    (term-send-raw-string (format "%s\n" command))
  )
)

(defvar rm/common-mix-commands
  (helm-build-in-buffer-source "Common mix commands"
    :data '(
            "MIX_ENV=test iex -S mix"
            "iex -S mix"
            "mix docs.dash"
            "mix deps.get"
            "mix compile --force"
            "mix credo --strict"
            "mix dialyzer"
            )
    :action 'rm/send-projectile-buffer-raw-string
  )
)

(defvar rm/common-cli-commands
  (helm-build-in-buffer-source "Common cli commands"
    :data '(
            "git commit --amend --no-edit"
            ;; "git diff" > to list of branches or last few commit hashes w/ messages
            ;; "gco" > to list of branches, clubhouse cards, and last few commit hashes w/ messages
            )
    :action 'rm/send-projectile-buffer-raw-string
  )
)

;; list of mix commands to immediately send "M-m"
(defun rm/helm-mix-commands ()
  "Helm interface to fire mix commands"
  (interactive)
  (helm :sources '( rm/common-mix-commands rm/common-cli-commands )
  :buffer "*helm mix commands*"
  )
)

(provide 'init-term)
;;; init-term.el ends here
