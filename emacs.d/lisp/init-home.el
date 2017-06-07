;;; init-home.el --- Emacs startup screen
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(find-file "~/Dropbox/todo/todo.org")
(split-window-right)
(find-file "~/dotfiles/emacs.d/lisp/todo.org")
(rm/projectile-run-term)

(load-theme 'atom-one-dark)

(provide 'init-home)
;;; init-home.el ends here
