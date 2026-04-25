;;; ~/dotfiles/emacs/.doom.d/autoload/fennel.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Supports running love2d games from Emacs
;; Creates a comint/inferior-lisp buffer for each project
;; Depends on projectile.

;;; Code:

(defun love-buffer-name-no-ax ()
  "A project-based buffer name for the love process. No asterisks included."
  (concat "love2d-" (projectile-project-name)))

(defun love-buffer-name ()
  "Wrap the name in the usual asterisks, to match the buffer created by `make-comint'."
  (concat "*" (love-buffer-name-no-ax) "*"))

;;;###autoload
(defun russ/run-love-for-project ()
  "Start a love 2d game for the current directory."
  (interactive)

  ;; move to project root
  (cd (projectile-project-root))

  ;; create process and buffer if they don't exist
  (when (not (comint-check-proc (love-buffer-name)))
    (let ((cmdlist (split-string "love .")))
     (set-buffer (apply #'make-comint
                        (love-buffer-name-no-ax)
                        (car cmdlist) nil (cdr cmdlist)))
     (inferior-lisp-mode)
     (setq inferior-lisp-buffer (love-buffer-name))
     (set (make-local-variable 'lisp-describe-sym-command) "(doc %s)\n")
     (set (make-local-variable 'inferior-lisp-prompt) ">> ")
     (set (make-local-variable 'lisp-arglist-command)
          fennel-arglist-command))))

;;;###autoload
(defun russ/love-kill-process-buffer ()
  "Destroy the love game, no prompts."
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
  "Destroy and recreate the love game."
  (interactive)
  ;; Kill inferior lisp buffer if there is one
  (russ/love-kill-process-buffer)

  ;; start a new love instance
  (russ/run-love-for-project))

;;;###autoload
(defun russ/love-kill-and-restart-via-tmux ()
  "Destroy and recreate the love game."
  (interactive)
  (shell-command-to-string "ralphie interrupt")
  (shell-command-to-string "ralphie fire love ."))

;;;###autoload
(defun russ/open-love-repl ()
  "Open a love repl. Start the love game first to create it."
  (interactive)

  (if (get-buffer-process inferior-lisp-buffer)
      (pop-to-buffer inferior-lisp-buffer)

    ;; start a new love instance
    (russ/run-love-for-project)
    ;; pop to buffer
    (pop-to-buffer inferior-lisp-buffer)))

;;;###autoload
(defun russ/fennel-hotswap-module (module-keyword)
  "Return a string of fennel to reload the `MODULE-KEYWORD' module."
  (format "%s\n" `(lume.hotswap ,module-keyword)))

(defun get-module-name ()
  "TODO expand to support namespaced module reload."
  (intern (concat ":" (file-name-base))))

;; ;;;###autoload
;; (defun russ/love-restart-in-place ()
;;   (interactive)
;;   (comint-send-string (inferior-lisp-proc)
;;                       (format "%s\n"
;;                               `(love.event.quit "restart"))))

;;;###autoload
(defun russ/love-module-reload ()
  "Reloads the visited module in place via lume.hotswap.
Supports lua and fennel."
  (interactive)
  ;; check if file needs to be saved first
  (comint-check-source buffer-file-name)

  ;; get module name
  (let* ((module (get-module-name)))
    ;; send to repl
    (comint-send-string (inferior-lisp-proc)
                        (russ/fennel-hotswap-module module))))

;;;###autoload
(defun russ/reload-scene ()
  (interactive)
  (russ/love-module-reload)
  (let* ((module (get-module-name)))
    ;; send to repl
    (comint-send-string (inferior-lisp-proc)
                        (format "%s\n" `(_G.load-scene ,module)))))

(comment
 fennel-mode
 (concat "hi" (projectile-project-root))
 (if nil
     (print "hi")
     (print "bye")
     (print "bye")
     (print "bye")))
