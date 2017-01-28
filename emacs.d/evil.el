;;; evil.el --- Evil mode configuration

;;; Commentary:

;; Evil mode, space as leader key

;;; Code:

(use-package evil
  :init
  (progn
    (setq evil-default-cursor t)

    (use-package evil-leader
      :init (global-evil-leader-mode)

      :config
      (progn
        (setq evil-leader/in-all-states t)

        (evil-leader/set-leader "<SPC>")

        (evil-leader/set-key
         "<SPC>" 'evil-switch-to-windows-last-buffer
         "n" 'neotree-find
         "g" 'magit-status
         "k" 'kill-buffer
         "S" 'helm-projectile-ag
         "s" 'split-window-below
         "v" 'split-window-right
         "/" 'helm-swoop
         )))

    (evil-mode 1))

  :config
  (progn
    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
  ))

(provide 'setup-evil)

;;; evil.el ends here
