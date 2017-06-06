;;; init-home.el --- Emacs startup screen
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(find-file "~/Dropbox/todo/todo.org")
(split-window-right)
(find-file "~/dotfiles/emacs.d/lisp/todo.org")
(ansi-term "/bin/zsh")

(load-theme 'atom-one-dark)

(provide 'init-home)
;;; init-home.el ends here
