;;; init-evil.el --- Evil mode config, including evil-leader
;;; Commentary:
;;; Code:

;; (use-package evil-visual-mark-mode)

(use-package evil
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-motion-state-map
         ("<tab>" . evil-indent-line)
         ("<return>" . nil)
         ("SPC" . nil)
         ("M-." . nil)
         ("(" . backward-sexp)
         (")" . forward-sexp)
         ("K" . nil)

         :map evil-normal-state-map
         ("S-<tab>" . org-cycle)
         ("<tab>" . evil-indent-line)
         ("<return>" . nil)
         ("M-." . nil)
         ("K" . nil)
         ("C-y" . yas-insert-snippet)
         ("C-z" . projectile-run-term)
         ("C-b" . eval-buffer)

         :map evil-visual-state-map
         ("g c" . evilnc-comment-or-uncomment-lines)

         :map evil-ex-map
         ("e" . helm-find-files)
         ("tn" . neotree-toggle)
         ("tap" . alchemist-mix-test-at-point)
         ("tl" . toggle-truncate-lines)
         ("lt" . alchemist-mix-rerun-last-test)
         ("ag" . helm-projectile-ag)
         ("Ag" . helm-projectile-ag)

         :map evil-insert-state-map
         ("C-y" . company-yasnippet)
         ("C-k" . nil)
         ("C-p" . nil)
         ("C-n" . nil)
         ("C-r" . nil)
         ("C-t" . nil)
         ("C-e" . nil)
         ("C-a" . nil)

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
      "<SPC>" 'evil-switch-to-windows-last-buffer ;; TODO this command doesn't toggle properly after helm-semantic-or-imenu
      "a" 'ace-window
      "b" 'helm-mini
      "c" 'evilnc-comment-or-uncomment-lines
      "e" 'helm-M-x
      "f" 'helm-find-files
      "i" 'popup-imenu
      "I" 'helm-imenu-anywhere
      "k" 'kill-buffer
      "l" 'alchemist-mix-rerun-last-test
      "n" 'neotree-find-current-file
      "N" 'neotree-reveal-current-buffer
      "o" 'projectile-multi-occur
      "p" 'helm-projectile
      "!" 'flycheck-list-errors
      "1" 'flycheck-list-errors
      "qn" 'neotree-toggle
      "qq" 'evil-window-delete
      "qw" 'evil-window-delete
      "qk" 'kill-this-buffer
      "qb" 'kill-this-buffer
      "qa" 'ace-delete-window
      "r" 'org-ctrl-c-ctrl-c
      "S" 'helm-projectile-ag
      "s" 'split-window-below
      "t" 'alchemist-project-toggle-file-and-tests
      "T" 'alchemist-mix-test-this-buffer
      "v" 'split-window-right
      "w" 'save-buffer
      "Wl" '(lambda () (interactive) (evil-window-move-far-right))
      "WL" '(lambda () (interactive) (evil-window-move-far-right))
      "Wh" '(lambda () (interactive) (evil-window-move-far-left))
      "WH" '(lambda () (interactive) (evil-window-move-far-left))
      "x" 'helm-M-x
      "=" 'balance-windows
      "-" 'split-window-below
      "_" 'split-window-below
      "\\" 'split-window-right
      "|" 'split-window-right
      ">" '(lambda () (interactive) (evil-window-increase-width 20))
      "<" '(lambda () (interactive) (evil-window-decrease-width 20))
      "y" 'yas-insert-snippet
      "z" 'projectile-run-term
    )
  )
)

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


    (with-eval-after-load 'evil
        (defalias #'forward-evil-word #'forward-evil-symbol))
  )
)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

(use-package evil-nerd-commenter)

(provide 'init-evil)
;;; init-evil.el ends here
