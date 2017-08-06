;;; init-home.el --- Emacs startup screen
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(find-file "~/Dropbox/todo/gtd.org")
(find-file-other-window "~/Dropbox/todo/inbox.org")
(emux-toggle-terminal-window)

(provide 'init-home)
;;; init-home.el ends here
