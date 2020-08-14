;;; ~/dotfiles/emacs/.doom.d/autoload/fennel.el -*- lexical-binding: t; -*-

;;;###autoload
;; Support opening emacs in a workspace
(defun russ/love-2d-fennel-repl ()
  "Based on `fennel-repl' from `fennel-mode'.
Hardcodes \"love .\" as the initial lisp command."
  (interactive)
  (if (get-buffer-process inferior-lisp-buffer)
      (pop-to-buffer inferior-lisp-buffer)
    (run-lisp "love .")
    (set (make-local-variable 'lisp-describe-sym-command) "(doc %s)\n")
    (set (make-local-variable 'inferior-lisp-prompt) ">> ")
    (set (make-local-variable 'lisp-arglist-command) fennel-arglist-command)))
