(use-package neotree
    :init
    (progn
      ;; Every time when the neotree window is opened, it will try to find current
      ;; file and jump to node.
      (setq-default neo-smart-open t)
      ;; Do not allow neotree to be the only open window
      (setq-default neo-dont-be-alone t))

    :config
    (progn
      ;; theme
      (setq neo-theme 'ascii)

      ;; evil mappings
      (evil-set-initial-state 'neotree-mode 'normal)

      (evil-define-key 'normal neotree-mode-map
      (kbd "RET") 'neotree-enter
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
      ))

      ;; follow with projectile
      (setq projectile-switch-project-action 'neotree-projectile-action)

      ;; neo vc integration
      (setq neo-vc-integration '(face char))

      ;; Patch to fix vc integration
      (defun neo-vc-for-node (node)
      (let* ((backend (vc-backend node))
        (vc-state (when backend (vc-state node backend))))
        ;; (message "%s %s %s" node backend vc-state)
        (cons (cdr (assoc vc-state neo-vc-state-char-alist))
          (cl-case vc-state
            (up-to-date       neo-vc-up-to-date-face)
            (edited           neo-vc-edited-face)
            (needs-update     neo-vc-needs-update-face)
            (needs-merge      neo-vc-needs-merge-face)
            (unlocked-changes neo-vc-unlocked-changes-face)
            (added            neo-vc-added-face)
            (removed          neo-vc-removed-face)
            (conflict         neo-vc-conflict-face)
            (missing          neo-vc-missing-face)
            (ignored          neo-vc-ignored-face)
            (unregistered     neo-vc-unregistered-face)
            (user             neo-vc-user-face)
            (t                neo-vc-default-face)))))
    )
