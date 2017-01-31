;;; projectile.el --- Projectile mode configuration

;;; Commentary:

;; Find File after project select.

;;; Code:

(use-package projectile
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-find-file)
    (projectile-mode)
    )
)

(provide 'setup-projectile)


;;; projectile.el ends here
