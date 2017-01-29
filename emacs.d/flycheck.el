;;; flycheck.el --- Flycheck mode configuration

;;; Commentary:

;; Linting.

;;; Code:

(use-package flycheck
  :config
  (global-flycheck-mode)

  ; Flycheck Mix Settings
  (use-package flycheck-mix
    :init
    (flycheck-mix-setup))

  ;; Flycheck Credo Settings
  (use-package flycheck-credo
    :init
    (flycheck-credo-setup))
  )

(provide 'setup-flycheck)

;;; flycheck.el ends here
