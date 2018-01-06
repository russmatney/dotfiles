;;; private/russ/autoload/tmux.el -*- lexical-binding: t; -*-


(defvar +russ/shell-command-stack '())

;;;###autoload
(defun +russ/tmux-repeat ()
  ""
  (interactive)
  (+tmux/run "!!\n"))

;;;###autoload
(defun +russ/tmux-cancel ()
  ""
  (interactive)
  (+tmux/run "^C"))

;;;###autoload
(defun +russ/tmux-send-q ()
  ""
  (interactive)
  (+tmux/run "q"))

;;;###autoload
(defun +russ/tmux-switch-client (&optional client)
  "Switch to active tmux session"
  (interactive
   (list (completing-read "Switch TMUX Client: "
         (mapcar (lambda (sess)
                   (plist-get sess :name))
                 (+tmux-list-sessions))
         )))
  (+tmux (concat "switch-client -t " client)))

;;;###autoload
(defun +russ/shell-command (&optional command)
  ""
  (interactive
   (list
    (completing-read "Fire shell command: "
                     +russ/shell-command-stack)))

  (setq +russ/shell-command-stack
        (delete-dups (cons command +russ/shell-command-stack)))
  (+tmux/run command))

;;;###autoload
(defun +russ/tmux-command (&optional command)
  ""
  (interactive
   (list
    (completing-read "Fire tmux command: "
                     +russ/shell-command-stack)))

  (setq +russ/shell-command-stack
        (delete-dups (cons command +russ/shell-command-stack)))
  (+tmux command))
