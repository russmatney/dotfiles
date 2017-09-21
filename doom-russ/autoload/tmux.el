;;; private/russ/autoload/tmux.el -*- lexical-binding: t; -*-


(defvar +russ/tmux-command-stack '())

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
(defun +russ/tmux-command (&optional command)
  ""
  (interactive
   (list
    (completing-read "Fire shell command: "
                     +russ/tmux-command-stack)))

  (setq +russ/tmux-command-stack
        (delete-dups (cons command +russ/tmux-command-stack)))
  (+tmux/run command))
