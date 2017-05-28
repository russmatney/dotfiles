;;; init-defaults.el --- Settings and general configuration
;;; Commentary:
;;;   thanks to:
;;;   https://stackoverflow.com/questions/19970814/emacs-how-to-load-an-ansi-term-buffer-at-startup
;;; Code:

(add-hook 'emacs-startup-hook
  (lambda ()
    (split-window-right)
    (find-file-other-window "~/Dropbox/todo/todo.org")
    (kill-buffer "*scratch*")
    (ansi-term)
  ))

(provide 'init-defaults)
;;; init-settings.el ends here
