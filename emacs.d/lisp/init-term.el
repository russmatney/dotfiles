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

;; TODO figure out what this does
;; (defadvice ansi-term (before force-zsh)
;;   (interactive (list "/bin/zsh")))
;; (ad-activate 'ansi-term)

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
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-r") nil)
  (define-key evil-insert-state-map (kbd "C-t") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-c") 'term-interrupt-subjob)
  (define-key evil-normal-state-map (kbd "C-c") 'term-interrupt-subjob)

  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "M-j") 'wc/helm-autojump)
  (define-key term-raw-map (kbd "M-g") 'wc/helm-git-branches)

  (define-key term-raw-map (kbd "s-v") 'term-paste)

  ;; (evil-define-key 'insert term-pager-break-map
  ;;   (kbd "ESC") 'term-pager-discard
  ;; )

  (evil-define-key 'normal term-raw-map
    (kbd "i") 'rm/evil-open-at-bottom
  )
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
  (setq history (shell-command-to-string "cat ~/.zsh_history"))
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

(provide 'init-term)
;;; init-term.el ends here
