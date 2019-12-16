;;; ~/dotfiles/emacs/.doom.d/autoload/russ.el -*- lexical-binding: t; -*-

;;;###autoload
(defun russ/open-org-file ()
  "Browse your org-dir."
  (interactive)
  (let ((org-dir "~/Dropbox/todo/"))
    (doom-project-browse org-dir)))

;;;###autoload
(defun russ/open-doom-file ()
  "Open a file in doom itself."
  (interactive)
  (let ((dir "~/.emacs.d/"))
    (doom-project-browse dir)))

;;;###autoload
(defun russ/open-dotfile ()
  "Open a file from my dotfiles itself."
  (interactive)
  (let ((dir "~/dotfiles/"))
    (doom-project-browse dir)))
