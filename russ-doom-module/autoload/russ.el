;;; private/russ/autoload/russ.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +russ/install-snippets ()
  "Install my snippets from https://github.com/russ/emacs-snippets into
private/russ/snippets."
  (interactive)
  (doom-fetch :github "russ/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'russ))))

;;;###autoload
(defun +russ/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(defmacro +russ-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+russ/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+russ/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+russ/find-in-templates "private/russ/autoload/russ" nil t)
;;;###autoload (autoload '+russ/browse-templates "private/russ/autoload/russ" nil t)
(+russ-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+russ/find-in-snippets "private/russ/autoload/russ" nil t)
;;;###autoload (autoload '+russ/browse-snippets "private/russ/autoload/russ" nil t)
(+russ-def-finder! snippets +russ-snippets-dir)

;;;###autoload (autoload '+russ/find-in-dotfiles "private/russ/autoload/russ" nil t)
;;;###autoload (autoload '+russ/browse-dotfiles "private/russ/autoload/russ" nil t)
(+russ-def-finder! dotfiles (expand-file-name "dotfiles" "~"))

;;;###autoload (autoload '+russ/find-in-emacsd "private/russ/autoload/russ" nil t)
;;;###autoload (autoload '+russ/browse-emacsd "private/russ/autoload/russ" nil t)
(+russ-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+russ/find-in-notes "private/russ/autoload/russ" nil t)
;;;###autoload (autoload '+russ/browse-notes "private/russ/autoload/russ" nil t)
(+russ-def-finder! notes +org-dir)
