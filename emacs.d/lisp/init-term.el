;;; init-term.el --- Configuration for
;;; Commentary:
;;;   much thanks to:
;;;     - http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;;;     - https://emacs.stackexchange.com/questions/328/how-to-override-keybindings-for-term
;;;     - http://oremacs.com/2015/01/01/three-ansi-term-tips/
;;;     - https://emacs.stackexchange.com/questions/18672/how-to-define-a-function-that-calls-a-console-process-using-ansi-term
;;;     - https://github.com/wpcarro/pc_settings/commit/aab701e10e52afec790b069a5e14c961c6f32307
;;; Code:

(defadvice ansi-term (before force-zsh)
  "Attempt to choose zsh everytime."
  (interactive (ansi-term "/bin/zsh")))
(ad-activate 'ansi-term)


(defun rm/toggle-terminal-window-focus ()
  "Toggle the cursor between the project term buffer and the last window selected."
  (interactive)
  (if (rm/is-term-window-p) (rm/select-previous-window)
    (rm/select-terminal-window)))

(defun rm/toggle-terminal-window-display ()
  "Toggle the display of the terminal window.
Should already exist before being toggled."
  (interactive)
  (if (rm/term-window-open-p) (rm/hide-terminal-window)
    (rm/display-terminal-buffer nil))
)

(defun rm/display-terminal-buffer-keep-focus ()
  "Display but do not focus on the project's root terminal buffer."
  (interactive)
  (rm/display-terminal-buffer nil))

(defun rm/switch-to-terminal-window ()
  "Switch to the project's root term instance.
Creates it if it doesn't exist.
If the window is already open, moves focus to that window.
Otherwise, opens the terminal in this window."
  (interactive)
  (rm/display-terminal-buffer t)
  (rm/select-terminal-window))

(defun rm/switch-this-window-to-terminal-window ()
  "Switch to the project's root term instance in the current window.
Creates it if it doesn't exist.
If the window is already open, moves focus to that window.
Otherwise, opens the terminal in this window."
  (interactive)
  (rm/display-terminal-buffer nil)
  (rm/select-terminal-window))

(defun rm/term-exec-hook ()
  "Hook to run when term exec hook run."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
)
(add-hook 'term-exec-hook 'rm/term-exec-hook)

;; force term-mode to expose the passed global binding
(defun expose-global-binding-in-term (binding)
  "Expose BINDING that is otherwise blocked/used by term mode."
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))

(defun rm/evil-open-at-bottom ()
  "Move the cursor to the bottom of the terminal when entering insert mode."
  (interactive)
  (end-of-buffer)
  (evil-insert-state 1)
  (term-send-raw-string "\b"))

(eval-after-load "term"
  '(progn
     ;; ensure that scrolling doesn't break on output
     (setq term-scroll-to-bottom-on-output t)))


(defun rm/term-mode-hook ()
  (goto-address-mode)
  (linum-mode -1)

  (setq window-max-chars-per-line 1000)
  ;; (setq term-scroll-show-maximum-output t)

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
  )
  (evil-define-key 'normal term-raw-map
    (kbd "C-r") nil
    (kbd "C-c") 'term-interrupt-subjob
    (kbd "C-r") 'wc/helm-shell-history
    (kbd "i") 'rm/evil-open-at-bottom
    (kbd ".") 'rm/repeat-last-shell-command
  )

  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "M-j") 'wc/helm-autojump)
  (define-key term-raw-map (kbd "M-g") 'wc/helm-git-branches)
  (define-key term-raw-map (kbd "s-v") 'term-paste)
)
(add-hook 'term-mode-hook 'rm/term-mode-hook)

(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))


(defun rm/repeat-last-shell-command ()
  "Rerun the last shell command in the local project terminal."
  (interactive)
  (rm/run-shell-command "!! "))

(defun rm/run-shell-command-from-minibuffer-action (command)
  "Open the mini-buffer for command input.
COMMAND is ignored.
This is a convenience function for helm."
  (interactive)
  (rm/run-shell-command-from-minibuffer))

(defun rm/run-shell-command-from-minibuffer ()
  "Run COMMAND in a `term' buffer."
  (interactive)
  (let* ((command (read-from-minibuffer "$ ")))
    (rm/run-shell-command command nil t)))



(defun rm/run-shell-command (command &optional start-new-session send-focus-to-term use-this-window)
  "Run a passed string as a CLI COMMAND in the project's local terminal.

The command is fired to either an existing terminal buffer
or a new one if none exists.  START-NEW-SESSION forces a new one.

SEND-FOCUS-TO-TERM selects the window once it is displayed,
otherwise focus remains in the current position.

USE-THIS-WINDOW displays the terminal buffer in the current window,
otherwise a dedicated side-window will be used."
  (rm/send-command-to-terminal (format "%s\n" command) start-new-session)
  (rm/display-terminal-buffer use-this-window)
  (if (or send-focus-to-term use-this-window) (rm/select-terminal-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-shell-command Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/start-new-terminal ()
  "Create a new terminal session."
  (projectile-with-default-dir (projectile-project-root)
    (set-buffer (make-term (replace-regexp-in-string "\*" "" (rm/new-term-buffer-name)) "/bin/zsh"))
    (term-mode)
    (term-char-mode)
  )
)

(defun rm/send-command-to-terminal (command &optional in-new-term)
  "COMMAND is the command to send.
IN-NEW-TERM is whether or not to create a new term buffer to run the command.

If in-new-term is t or the buffer does not exist,
Creates a new terminal session in the current projectile root,
and fires the passed command.

If in-new-term is nil and the buffer exists,
Sends the command to the buffer via term-send-raw-string."
  (if (or in-new-term (not (rm/term-session-exists-p))) (rm/start-new-terminal))
  ;; should get 'newest' term-buffer name or it should be returned from rm/start-new-terminal
  (set-buffer (rm/local-term-buffer-name))
  (term-send-raw-string command)
)

(defun rm/display-terminal-buffer (use-this-window)
  "USE-THIS-WINDOW is a boolean flag.

If the current window is the terminal window, do nothing.
If use-this-window is t and there is not an open terminal window,
the current window displays the terminal window.
Otherwise, the terminal is displayed in the dedicated side bar."
  (cond ((rm/is-term-window-p) ())
        (use-this-window (unless (rm/term-window-open-p) (rm/show-terminal-this-window)))
        (t (rm/show-terminal-side-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Buffer Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/local-term-buffer-name ()
  "Return a name for a terminal buffer based on the projectile project it is in."
  (concat "*term " (projectile-project-name) "*"))

(defun rm/new-term-buffer-name ()
  "Return a new buffer name for the current context.  If one exists, append`<n>`."
  (generate-new-buffer-name
    (rm/local-term-buffer-name)))

(defun rm/get-term-buffer ()
  "Return the current local term buffer."
  (get-buffer (rm/local-term-buffer-name)))

(defun rm/term-session-exists-p ()
  "Return non-nil if a session for the current context exists."
  (get-buffer (rm/local-term-buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Window Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/is-term-window-p ()
  "Return non-nil if the current window is a *term window."
  (string-prefix-p "*term " (buffer-name (current-buffer))))

(defun rm/term-window-open-p ()
  "Return non-nil if the project's term window is already open."
  (if (get-buffer-window (rm/local-term-buffer-name)) t
    nil))

(defun rm/hide-terminal-window ()
  "Deletes the terminal window."
  (delete-window (rm/get-term-window)))

(setq shackle-rules '(("\\`\\*term.*?\\*\\'" :regexp t :align right :ratio 0.3)))
(shackle-mode)

(defun rm/show-terminal-side-window ()
  "Crashes if a terminal session does not exist."
  (display-buffer (get-buffer (rm/local-term-buffer-name))))

(defun rm/show-terminal-this-window ()
  "Crashes if a terminal session does not exist."
  (display-buffer-same-window (get-buffer (rm/local-term-buffer-name)) nil))

(defun rm/get-term-window ()
  "Gets the window for terminal buffer."
  (get-buffer-window (rm/local-term-buffer-name) t))

(defun rm/select-terminal-window ()
  "Crashes if a terminal session does not exist."
  (select-window (rm/get-term-window))
  (evil-insert 1))

(defun rm/select-previous-window ()
  (interactive)
  (evil-window-mru)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Window Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/term-scroll-page-up ()
  "Scroll the term window up."
  (interactive)
  (setq other-window-scroll-buffer (rm/get-term-buffer))
  (scroll-other-window))

(defun rm/term-scroll-page-down ()
  "Scroll the term window down."
  (interactive)
  (setq other-window-scroll-buffer (rm/get-term-buffer))
  (scroll-other-window-down))


(provide 'init-term)
;;; init-term.el ends here
