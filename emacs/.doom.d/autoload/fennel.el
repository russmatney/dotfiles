;;; ~/dotfiles/emacs/.doom.d/autoload/fennel.el -*- lexical-binding: t; -*-

;; Supports running love2d games from emacs
;; Creates a comint/inferior-lisp buffer for each project
;; Depends on projectile.

(defun love-buffer-name-no-ax ()
  (concat "love2d-" (projectile-project-name)))

(defun love-buffer-name ()
  (concat "*" (love-buffer-name-no-ax) "*"))

;;;###autoload
(defun russ/run-love-for-project ()
  "Starts a love 2d game in the current directory"
  (interactive)

  ;; move to project root
  (cd (projectile-project-root))

  ;; create process and buffer if they don't exist
  (let ((cmd "love ."))
    (if (not (comint-check-proc (love-buffer-name)))
        (let ((cmdlist (split-string cmd)))
          (set-buffer (apply #'make-comint
                             (love-buffer-name-no-ax)
                             (car cmdlist) nil (cdr cmdlist)))
          (inferior-lisp-mode)
          (setq inferior-lisp-buffer (love-buffer-name))
          (set (make-local-variable 'lisp-describe-sym-command) "(doc %s)\n")
          (set (make-local-variable 'inferior-lisp-prompt) ">> ")
          (set (make-local-variable 'lisp-arglist-command)
               fennel-arglist-command)))))

;;;###autoload
(defun russ/love-kill-process-buffer ()
  (interactive)
  (when inferior-lisp-buffer

    ;; remove confirmation for process buffers
    (setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

    (kill-buffer inferior-lisp-buffer)

    ;; add confirmation back for other process buffers
    (add-to-list 'kill-buffer-query-functions 'process-kill-buffer-query-function)))

;;;###autoload
(defun russ/love-kill-and-restart ()
  (interactive)
  ;; Kill inferior lisp buffer if there is one
  (russ/love-kill-process-buffer)

  ;; start a new love instance
  (russ/run-love-for-project))


;;;###autoload
(defun russ/open-love-repl ()
  (interactive)

  (if (get-buffer-process inferior-lisp-buffer)
      (pop-to-buffer inferior-lisp-buffer)

    ;; start a new love instance
    (russ/run-love-for-project)
    ;; pop to buffer
    (pop-to-buffer inferior-lisp-buffer)))

;;;###autoload
(defun russ/fennel-hotswap-module (module-keyword)
  "Return a string of the code to reload the `module-keyword' module."
  (format "%s\n" `(lume.hotswap ,module-keyword)))

(defun get-module-name ()
  "Ask for the name of a module for the current file; returns keyword.

TODO expand to support namespaced module reload."
  (intern (concat ":" (file-name-base))))

;;;###autoload
(defun russ/love-module-reload ()
  (interactive)
  ;; check if file needs to be saved first
  (comint-check-source buffer-file-name)

  ;; get module name
  (let* ((module (get-module-name)))

    ;; send to repl
    (comint-send-string (inferior-lisp-proc) (fennel-hotswap-module module))))

(comment
 fennel-mode
 (concat "hi" (projectile-project-root))
 (if nil
     (print "hi")
     (print "bye")
     (print "bye")
     (print "bye")
   )
 )
