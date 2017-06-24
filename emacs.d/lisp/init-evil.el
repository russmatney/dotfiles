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
         ("C-z" . nil)
         ("C-z C-x" . rm/helm-shell-commands)

         :map evil-normal-state-map
         ("S-<tab>" . org-cycle)
         ("<tab>" . evil-indent-line)
         ("<return>" . nil)
         ("M-." . nil)
         ("K" . nil)
         ("C-y" . yas-insert-snippet)
         ("C-b" . eval-buffer)
         ("C-z" . nil)
         ("C-SPC" . wc/switch-to-mru-buffer)
         ;; ("C-n" . neotree-reveal-current-buffer)
         ("C-n" . neotree-toggle) ;; TODO toggle on at project root
         ("C-t" . rm/toggle-terminal-window-display)
         ("C-z C-s" . rm/switch-this-window-to-terminal-window)
         ("C-z C-z" . rm/toggle-terminal-window-focus)
         ("C-z C-o" . rm/display-terminal-buffer-keep-focus)
         ("C-z C-x" . rm/helm-shell-commands)
         ("C-z C-." . rm/repeat-last-shell-command)
         ("C-q C-q" . evil-window-delete)
         ("C-q C-n" . neotree-toggle)
         ("C-u" . evil-scroll-up)
         ("C-d" . evil-scroll-down)
         ("C-z C-k" . rm/term-scroll-page-up)
         ("C-z C-j" . rm/term-scroll-page-down)
         ("n" . rm/evil-search-next-and-center)
         ("N" . rm/evil-search-previous-and-center)
         ("M-s" . rm/helm-term-buffers)

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
         ("C-n" . neotree-toggle)
         ("C-r" . nil)
         ("C-t" . nil)
         ("C-e" . nil)
         ("C-a" . nil)
         ("C-z" . nil)
         ("C-q C-q" . evil-window-delete)
         ("C-q C-n" . neotree-toggle)
         ("C-n" . neotree-toggle)
         ("C-u" . evil-scroll-up)
         ("C-d" . evil-scroll-down)
         ("C-z C-z" . rm/toggle-terminal-window-focus)
         ("C-z C-k" . rm/term-scroll-page-up)
         ("C-z C-j" . rm/term-scroll-page-down)
         ("C-SPC" . wc/switch-to-mru-buffer)
         ("M-s" . rm/helm-term-buffers)
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
      "<SPC>" 'wc/switch-to-mru-buffer
      "a" 'ace-window
      "b" 'helm-mini
      "c" 'evilnc-comment-or-uncomment-lines
      "d" 'vc-diff
      "e" 'helm-M-x
      "f" 'helm-find-files
      "h" 'evil-window-delete
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
      "]" 'flycheck-next-error
      "[" 'flycheck-previous-error
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
      ;; "t" 'rm/alchemist-project-toggle-file-and-tests
      "m" 'alchemist-macroexpand
      "T" 'org-babel-tangle
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
      "y" 'yas-insert-snippet
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

  ;; evil cursor colors
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  ;; (setq evil-move-cursor-back nil)
)

(defun rm/evil-search-next-and-center ()
    (interactive)
  (call-interactively 'evil-search-next)
  (call-interactively 'evil-scroll-line-to-center))

(defun rm/evil-search-previous-and-center ()
    (interactive)
  (call-interactively 'evil-search-previous)
  (call-interactively 'evil-scroll-line-to-center))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

(use-package evil-nerd-commenter)

(provide 'init-evil)
;;; init-evil.el ends here
