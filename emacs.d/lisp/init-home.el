;;; init-home.el --- Emacs startup screen
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(find-file "~/Dropbox/todo/todo.org")
(split-window-right)
(find-file "~/dotfiles/emacs.d/lisp/todo.org")
;; (emux-run-shell-command "ls" nil t)

(load-theme 'atom-one-dark)
;; (load-theme 'github)
;; (load-theme 'smart-mode-line-light)

(provide 'init-home)
;;; init-home.el ends here
