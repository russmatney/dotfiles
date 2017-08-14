;;; private/russ/config.el -*- lexical-binding: t; -*-

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands) ; my custom ex commands
  (load! +helm-mini)
  (load! +org)
  )

(defvar +russ-dir
  (file-name-directory load-file-name))

(defvar +russ-snippets-dir
  (expand-file-name "snippets/" +russ-dir))

;;
(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+russ-snippets-dir)
                (delete 'yas-installed-snippets-dir
                        yas-snippet-dirs))))


(after! company
  (setq company-idle-delay 0))

(add-hook! elixir-mode
  (flycheck-mode))

(add-hook! elm-mode
  (flycheck-mode))

(add-hook! rust-mode
  (flycheck-mode))

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))
