;;; private/russ/config.el -*- lexical-binding: t; -*-

(when (featurep 'evil)
  (load! +bindings)
  (load! +evil)
  (load! +git)
  (load! +kb-fixes)
  (load! +leader)
  (load! +commands)
  (load! +helm)
  (load! +helm-mini)
  (load! +company)
  (load! +neotree)
  (load! +org))

(defvar +russ-dir
  (file-name-directory load-file-name))

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))

(setq +ivy-buffer-icons t)


;; elm
(add-hook! elm-mode
  (flycheck-mode))

;; rust
(add-hook! rust-mode
  (flycheck-mode))

;; elixir
(add-hook! elixir-mode
  (flycheck-mode))

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))


(add-hook! 'before-save-hook 'whitespace-cleanup)
