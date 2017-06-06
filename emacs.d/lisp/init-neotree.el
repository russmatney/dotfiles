;;; init-neotree.el --- Neotree config
;;; Commentary:
;;; Code:

(use-package neotree
  :init
  (setq neo-smart-open t)

  :config
  (progn

    (setq-default neo-show-hidden-files t)
    (setq-default neo-window-fixed-size nil)
    (setq-default neo-window-width 40)
    (setq neo-force-change-root t)

    (defun neotree-find-current-file ()
      "Reveal current buffer in Neotree."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))

        (neotree-show)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name))))
      (message "Could not find git project root.")))

    (defun neotree-reveal-current-buffer ()
      "Reveal current buffer in Neotree."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))

        (neotree-show)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)
                  (evil-window-mru)))
      (message "Could not find git project root."))))


    ;; theme
    (use-package all-the-icons
      ;; install fonts from this package too
    )
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))


    (defun helm-ag-neotree-node ()
      "Run Helm-ag on Neotree directory."
      (interactive)
      (let* ((search-root (neo-buffer--get-filename-current-line)))
        (if search-root
            ;; search directory
            (progn
              (evil-window-right 1)
              (helm-ag search-root))
          (message "Could not find directory at point."))))

    ;; evil mappings
    (evil-set-initial-state 'neotree-mode 'normal)

    (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    ;;(kbd "TAB") 'neotree-enter
    (kbd "c")   'neotree-create-node
    (kbd "r")   'neotree-rename-node
    (kbd "d")   'neotree-delete-node
    (kbd "j")   'neotree-next-line
    (kbd "k")   'neotree-previous-line
    (kbd "R")   'neotree-refresh
    (kbd "C")   'neotree-change-root
    (kbd "H")   'neotree-hidden-file-toggle
    (kbd "q")   'neotree-hide
    (kbd "s")   'neotree-enter-horizontal-split
    (kbd "v")   'neotree-enter-vertical-split
    (kbd "p")   'helm-ag-neotree-node
    ;; (kbd "x")   'neo-buffer--toggle-expand
    )
  )
)


(provide 'init-neotree)
;;; init-neotree.el ends here
