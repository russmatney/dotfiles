;;; evil.el --- Evil mode configuration

;;; Commentary:

;; Evil mode, space as leader key

;;; Code:

(use-package evil
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-insert-state-map
         ("<escape>" . evil-force-normal-state)

         :map evil-motion-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("SPC" . nil)
         ("M-." . nil)
         ("/" . helm-swoop)
         ("*" . helm-swoop)

         :map evil-normal-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("M-." . nil)
         ("/" . helm-swoop)
         ("*" . helm-swoop)
         ("C-p" . helm-projectile)

         :map evil-visual-state-map
         ("g c" . comment-or-uncomment-region)

         :map evil-ex-map
         ("e" . helm-find-files)
         ("b" . helm-buffers-list)
         ("tb" . alchemist-mix-test-this-buffer)
         ("tap" . alchemist-mix-test-at-point)
         ("lt" . alchemist-mix-rerun-last-test)
        )

  :init
  (progn
    (setq evil-default-cursor t)
    (setq evil-shift-width 2)

    (use-package evil-leader
      :init (global-evil-leader-mode)

      :config
      (progn
        (setq evil-leader/in-all-states t)

        (evil-leader/set-leader "<SPC>")

        (evil-leader/set-key
         "<SPC>" 'evil-switch-to-windows-last-buffer
         "c" 'comment-or-uncomment-region
         "n" 'neotree-find
         "g" 'magit-status
         "k" 'kill-buffer
         "b" 'helm-mini
         "S" 'helm-projectile-ag
         "s" 'split-window-below
         "v" 'split-window-right
         "x" 'alchemist-mix
         "r" 'alchemist-mix-rerun-last-test
         "t" 'alchemist-project-toggle-file-and-tests
         "T" 'alchemist-project-toggle-file-and-tests-other-window
         "d" 'alchemist-help-search-at-point
         "=" 'balance-windows
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

    ;; ensure company completion via number repeats as it should
    ;;(evil-declare-change-repeat 'company-complete-number)

    ;; Evil Match-it
    (use-package evil-matchit
      :config (global-evil-matchit-mode t))

    ;; Evil Surround
    (use-package evil-surround
      :config (global-evil-surround-mode t))

  )
)

(provide 'setup-evil)

;;; evil.el ends here
