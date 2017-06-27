;;; emux.el --- An Emacs Terminal Multiplexer
;;; Commentary:


;;; TODO: easier toggle into/out of term
;;; TODO: expose send custom func to term

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-term-exec-hook ()
  "Hook to run when term exec hook run."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'emux-term-exec-hook)

(eval-after-load "term"
  '(progn
     (setq term-scroll-to-bottom-on-output t)))

(defun emux-term-mode-hook ()
  "Config term mode bindings."
  (goto-address-mode)
  (linum-mode -1)

  (define-key term-raw-map (kbd "s-v") 'term-paste)
  )
(add-hook 'term-mode-hook 'emux-term-mode-hook)

(defadvice ansi-term (before force-zsh)
  "Attempt to choose zsh everytime."
  (interactive (ansi-term "/bin/zsh")))
(ad-activate 'ansi-term)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term window toggling and movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-toggle-terminal-window-display ()
  "Toggle the display of the terminal window."
  (interactive)
  (if (emux-term-window-open-p) (emux-hide-terminal-window)
    (emux-display-terminal-buffer nil)))

(defun emux-toggle-terminal-window-focus ()
  "Toggle the cursor position between the project term buffer and the last window selected."
  (interactive)
  (if (emux-term-window-selected-p) (emux-select-previous-window)
    (emux-switch-to-terminal-window)))


;; TODO toggle focus that opens + focuses/closes + refocuses

(defun emux-switch-to-terminal-window ()
  "Switch to the project's root term instance.
Create it if it doesn't exist.

If the window is already open, select that window.
Otherwise, display the terminal and then select it."
  (interactive)
  (emux-display-terminal-buffer nil)
  (emux-select-terminal-window))

(defun emux-term-on-right ()
  "Displays the terminal buffer on the right side of the frame."
  (interactive)
  (emux-set-term-position 'right))

(defun emux-term-on-left ()
  "Displays the terminal buffer on the left side of the frame."
  (interactive)
  (emux-set-term-position 'left))

(defun emux-term-on-bottom ()
  "Displays the terminal buffer along the top of the frame."
  (interactive)
  (emux-set-term-position 'below))

(defun emux-term-on-top ()
  "Displays the terminal buffer along the top of the frame."
  (interactive)
  (emux-set-term-position 'above))

;; helper
(defun emux-set-term-position (position)
  "Displays the terminal buffer in the POSITION passed.

Position is one of 'left 'right 'above 'below,
and is eventually passed to shackle.

Handles reselecting the terminal buffer if it was selected before the move."
  (interactive)
  (if (and (eq position emux-term-alignment) (emux-term-window-open-p))
      (progn
        (emux-select-previous-window)
        (emux-hide-terminal-window))
    (let ((reselect (emux-term-window-selected-p)))
      (setq emux-term-alignment position)
      (emux-hide-terminal-window)
      (emux-display-terminal-buffer nil)
      (if reselect (emux-select-terminal-window)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Window Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-term-scroll-page-up ()
  "Scroll the term window up."
  (interactive)
  (setq other-window-scroll-buffer (emux-get-term-buffer))
  (scroll-other-window))

(defun emux-term-scroll-page-down ()
  "Scroll the term window down."
  (interactive)
  (setq other-window-scroll-buffer (emux-get-term-buffer))
  (scroll-other-window-down))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Term command sending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-run-shell-command (command &optional force-new-term select-term-window use-this-window)
  "Run a passed string as a COMMAND in the project's local terminal buffer.

The command is fired to either an existing terminal buffer
or a new one if none exists.  FORCE-NEW-TERM forces a new one.

SELECT-TERM-WINDOW selects the window once it is displayed,
otherwise focus remains in the current position.

USE-THIS-WINDOW displays the terminal buffer in the current window,
otherwise a dedicated side-window will be used."
  (emux-send-command-to-terminal (format "%s\n" command) force-new-term)
  (emux-display-terminal-buffer use-this-window)
  (if (or select-term-window use-this-window) (emux-select-terminal-window)))

(defun emux-repeat-last-shell-command ()
  "Rerun the last shell command in the local project terminal."
  (interactive)
  (emux-run-shell-command "!! "))

(defun emux-run-shell-command-from-minibuffer ()
  "Run COMMAND in a `term' buffer.

Select the window with the term buffer after running the command."
  (interactive)
  (let* ((command (read-from-minibuffer "$ ")))
    (emux-run-shell-command command nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private startup, command, display helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-start-new-terminal ()
  "Create a new terminal session in a buffer.
Return the created buffer."
  (projectile-with-default-dir (projectile-project-root)
    (let ((buffer
           (make-term (replace-regexp-in-string "\*" "" (emux-new-term-buffer-name)) "/bin/zsh")))
      (set-buffer buffer)
      (term-mode)
      (term-char-mode)
      buffer)))


(defun emux-send-command-to-terminal (command &optional force-new-term)
  "COMMAND is the command to send.
FORCE-NEW-TERM is whether or not to force the
creation of a new term buffer to run the command.

If FORCE-NEW-TERM is t or the buffer does not exist,
a new terminal session is started in the current projectile root,
and is sent the passed command.

If FORCE-NEW-TERM is nil and the buffer exists,
Sends the command to the buffer via term-send-raw-string."
  (let ((buffer
         (if (or force-new-term (not (emux-term-session-exists-p))) (emux-start-new-terminal)
           (emux-get-term-buffer))))

    (set-buffer buffer)
    (term-send-raw-string command)))


(defun emux-display-terminal-buffer (use-this-window)
  "USE-THIS-WINDOW is a boolean flag.

Get the buffer for the current projectile project,
create one if one does not exist.

If USE-THIS-WINDOW is non-nil, display the term buffer in this window.
Otherwise, use `display-buffer`.

The buffer is always dedicated via `set-window-dedicated-p`."
  (let ((buffer
         (if (not (emux-term-session-exists-p)) (emux-start-new-terminal)
           (emux-get-term-buffer))))

    (if use-this-window
        (display-buffer-same-window buffer nil)
      (display-buffer buffer))

    (set-window-dedicated-p (get-buffer-window buffer) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Buffer Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-local-term-buffer-name ()
  "Return a name for a terminal buffer based on the current buffer's projectile project."
  (concat "*term " (projectile-project-name) "*"))

(defun emux-new-term-buffer-name ()
  "Return a new buffer name for the current project.
If one exists, `<n>` is appended."
  (generate-new-buffer-name
    (emux-local-term-buffer-name)))

(defun emux-get-term-buffer ()
  "Return the current project term buffer."
  (get-buffer (emux-local-term-buffer-name)))

(defun emux-term-session-exists-p ()
  "Return non-nil if a local project term buffer already exists."
  (get-buffer (emux-local-term-buffer-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term Window Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emux-term-window-selected-p ()
  "Return non-nil if the current window is a *term window."
  (string-prefix-p "*term " (buffer-name (current-buffer))))

(defun emux-term-window-open-p ()
  "Return `t` if the project's term window is already open."
  (if (get-buffer-window (emux-local-term-buffer-name))
      t
    nil))

(defun emux-hide-terminal-window ()
  "Deletes the project term window (not the buffer)."
  (if (emux-term-window-open-p)
      (delete-window (emux-get-term-window))))

(defun emux-get-term-window ()
  "Gets the window for the local project term buffer."
  (get-buffer-window (emux-local-term-buffer-name) t))

(defun emux-select-terminal-window ()
  "Select the window with the local project term buffer."
  (if (not (emux-term-window-open-p))
      (message "No term window ready to select.")
    (select-window (emux-get-term-window))
    (evil-insert 1)))

(defun emux-select-previous-window ()
  "Move the point to the window that was active before the current."
  (interactive)
  (evil-window-mru)
)



(provide 'emux)
;;; emux.el ends here
