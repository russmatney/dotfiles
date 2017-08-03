;;; init-projectile.el --- Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-find-file)
    (projectile-mode)))


(provide 'init-projectile)
;;; init-projectile.el ends here
