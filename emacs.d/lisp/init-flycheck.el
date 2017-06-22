;;; init-flycheck.el --- Flycheck config
;;; Commentary:
;;; Code:

(use-package flycheck
  :config
  (global-flycheck-mode)

  (require 'flycheck-mix)
  (add-to-list 'flycheck-checkers 'elixir-mix t)

  (require 'flycheck-credo)
  (setq flycheck-elixir-credo-strict t)
  (add-to-list 'flycheck-checkers 'elixir-credo t)

  (flycheck-add-next-checker 'elixir-mix '(error . elixir-credo))


  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.2)))

  (setq flycheck-display-errors-delay 0)
)


(provide 'init-flycheck)
;;; init-flycheck.el ends here
