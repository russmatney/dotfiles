;;; alchemist.el --- Alchemist mode configuration

;;; Commentary:

;; Alchemist provides elixir happiness.

;;; Code:

(use-package alchemist
  :config
    (setq alchemist-hooks-test-on-save t)
    (setq alchemist-hooks-compile-on-save t))

(provide 'setup-alchemist)

;;; alchemist.el ends here
