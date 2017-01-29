;;; alchemist.el --- Alchemist mode configuration

;;; Commentary:

;; Alchemist provides elixir happiness.

;;; Code:

(use-package alchemist
  :config
    (setq alchemist-goto-elixir-source-dir "/usr/local/share/src/elixir")
    (setq alchemist-goto-erlang-source-dir "/usr/local/share/src/otp")

    (setq alchemist-test-display-compilation-output t)
    (setq alchemist-hooks-test-on-save t)
    (setq alchemist-hooks-compile-on-save t)

    ;; fix to return from erlang dives
    (defun custom-erlang-mode-hook ()
        "Jump to and from Elixir, Erlang, Elixir files."
        (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
    (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)

    )

(provide 'setup-alchemist)

;;; alchemist.el ends here
