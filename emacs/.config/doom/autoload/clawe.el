;;; ../../dotfiles/emacs/.config/doom/autoload/clawe.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(setq clawe/ingest-on-save t)

;;;###autoload
(defun clawe/toggle-ingest-on-save ()
  (interactive)
  (let ((new-val (not clawe/ingest-on-save)))
    (print! "setting clawe ingest-on-save: %s" new-val)
    (setq clawe/ingest-on-save new-val)))

;;;###autoload
(defun clawe/doctor-ingest-this-file ()
  (interactive)
  (if (not clawe/ingest-on-save)
      (message "ingest on save disabled, nothing doing")

    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((path (buffer-file-name (buffer-base-buffer))))
      (message (concat "doctor ingesting file " path))
      ;; consider 'without-popups' or naming and ignoring a common clawebb buffer
      (async-shell-command
       (concat "clawebb -x clawe.doctor/ingest-file --path " path)
       ;; TODO add random int/timestamp to this buffer name
       ;; TODO add filename to command
       (concat "*clawebb-" (file-name-base (buffer-file-name)) "*")))))

(comment
 (message "hi")
 (clawe/doctor-ingest-this-file)
 (file-name-base) (buffer-file-name)
 )

;;;###autoload
(defun blog/publish-this-org-file ()
  (interactive)
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((path (buffer-file-name (buffer-base-buffer))))
    (message (concat "blog publishing file " path))
    ;; consider 'without-popups' or naming and ignoring a common clawebb buffer
    (async-shell-command
     (concat "bb --config ~/russmatney/blog/bb.edn -x blog.garden/generate-post --path " path)
     ;; TODO add random int/timestamp to this buffer name
     ;; TODO add filename to command
     (concat "*blogbb-" (file-name-base (buffer-file-name)) "*"))))

(comment
 (message "hi")
 (blog/publish-this-org-file)
 (file-name-base) (buffer-file-name)
 )
