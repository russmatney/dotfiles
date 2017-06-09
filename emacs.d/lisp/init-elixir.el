;;; init-elixir.el --- Elixir config
;;; Commentary:
;;; Code:

(use-package alchemist
  :config
    (setq alchemist-goto-elixir-source-dir "/usr/local/share/src/elixir")
    (setq alchemist-goto-erlang-source-dir "/usr/local/share/src/otp")

    (setq alchemist-test-display-compilation-output t)
    ;;(setq alchemist-hooks-test-on-save t)
    ;;(setq alchemist-hooks-compile-on-save t)

    ;; fix to return from erlang dives
    (defun custom-erlang-mode-hook ()
        "Jump to and from Elixir, Erlang, Elixir files."
        (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
    (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
)

(add-to-list 'display-buffer-alist
             `(,(rx bos (or "*alchemist test report*"
                            "*alchemist mix*"
                            "*alchemist help*"))
                    (display-buffer-reuse-window)
                    (inhibit-switch-frame t)
                    (reusable-frames . visible)))

(defun rm/alchemist-project-toggle-file-and-tests ()
  "Toggle between a file and its tests in the current window."
  (interactive)
  (if (alchemist-utils-test-file-p)
      (alchemist-project-open-file-for-current-tests 'find-file)
    (rm/alchemist-project-open-tests-for-current-file 'find-file)))

(defun rm/alchemist-project-open-tests-for-current-file (opener)
  "Visit the test file for the current buffer with OPENER."
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "^web/" "test/" filename))
         (filename (replace-regexp-in-string "^apps/\\(.*\\)/lib/" "apps/\\1/test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (if (file-exists-p filename)
        (funcall opener filename)
      (if (y-or-n-p "No test file found; create one now?")
          (alchemist-project--create-test-for-current-file
           filename (current-buffer))
        (message "No test file found.")))))

(provide 'init-elixir)
;;; init-elixir.el ends here
