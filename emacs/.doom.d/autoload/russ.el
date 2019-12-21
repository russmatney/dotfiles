;;; ~/dotfiles/emacs/.doom.d/autoload/russ.el -*- lexical-binding: t; -*-

;;;###autoload
(defun russ/open-emacs-config-file ()
  "Browse your org-dir."
  (interactive)
  (let ((org-dir "~/.doom.d"))
    (doom-project-find-file org-dir)))

;;;###autoload
(defun russ/open-org-file ()
  "Browse your org-dir."
  (interactive)
  (let ((org-dir "~/Dropbox/todo/"))
    (doom-project-find-file org-dir)))

;;;###autoload
(defun russ/open-doom-file ()
  "Open a file in doom itself."
  (interactive)
  (let ((dir "~/.emacs.d/"))
    (doom-project-find-file dir)))

;;;###autoload
(defun russ/open-dotfile ()
  "Open a file from my dotfiles itself."
  (interactive)
  (let ((dir "~/dotfiles/"))
    (doom-project-find-file dir)))
