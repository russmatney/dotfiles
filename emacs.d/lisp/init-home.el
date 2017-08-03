;;; init-home.el --- Emacs startup screen
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(find-file "~/dotfiles/emacs.d/lisp/todo.org")
(emux-toggle-terminal-window)

(provide 'init-home)
;;; init-home.el ends here
