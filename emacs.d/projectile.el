;;; projectile.el --- Projectile mode configuration

;;; Commentary:

;; Find File after project select, followed by neotree update.

;;; Code:

(use-package projectile
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-find-file)
    (add-hook 'projectile-after-switch-project-hook 'neotree-projectile-action)
    (projectile-mode)
    )
)

(provide 'setup-projectile)


;;; projectile.el ends here
