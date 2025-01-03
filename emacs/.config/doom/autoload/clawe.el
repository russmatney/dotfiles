;;; ../../dotfiles/emacs/.config/doom/autoload/clawe.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;###autoload
(defun clawe/doctor-ingest-this-file ()
  (interactive)
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((path (buffer-file-name (buffer-base-buffer))))
    (message (concat "doctor ingesting file " path))
    ;; consider 'without-popups' or naming and ignoring a common clawebb buffer
    (async-shell-command
     (concat "clawebb -x clawe.doctor/ingest-file --path " path)
     ;; TODO add random int/timestamp to this buffer name
     ;; TODO add filename to command
     (concat "*clawebb-" (file-name-base (buffer-file-name)) "*"))))

(comment
 (message "hi")
 (clawe/doctor-ingest-this-file)
 (file-name-base) (buffer-file-name)
 )
