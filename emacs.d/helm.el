;;; init-helm.el --- Helm configuration

;;; Commentary:

;;; Code:

(use-package helm
  :bind (
    ("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files)
    ("C-x f" . helm-projectile)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    ("C-x C-b" . helm-buffers-list)

    :map helm-map
    ([tab] . helm-next-source)
    ("C-j" . helm-next-line)
    ("C-k" . helm-previous-line)
    ("C-?" . describe-key)
    ([escape] . helm-keyboard-quit)

    :map helm-find-files-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)

    :map helm-read-file-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)
  )

  :init
  (progn
    (helm-mode 1))

  :config
  (progn
    (setq helm-buffers-fuzzy-matching t helm-recentf-fuzzy-match t)

    (setq helm-semantic-fuzzy-match t helm-imenu-fuzzy-match t)

    (setq helm-locate-fuzzy-match t)


    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on))
    )
  )
)


(provide 'init-helm)

;;; helm.el ends here
