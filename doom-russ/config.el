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
  (load! +helm-descbinds)
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

(set-register ?d '(file . "~/dotfiles/"))
(set-register ?u '(file . "~/projects/urbint/"))

(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))

(setq +ivy-buffer-icons t)


(add-to-list 'auto-mode-alist
             '("rc\\'" . (lambda () (conf-mode)))
             '("\\.rkt\\'" . (lambda () (geiser))))

(def-package! racket-mode
  :mode "\\.rkt\\'"
  :config
    (geiser-mode)
  )


;; elm
(add-hook! elm-mode
  (flycheck-mode))

;; rust
(add-hook! rust-mode
  (flycheck-mode)
  (rainbow-delimiters-mode)
  )

;; elixir
(add-hook! elixir-mode
  (flycheck-mode)
  (turn-off-smartparens-mode)
  (rainbow-delimiters-mode)
  t)

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))


;; emacs-lisp
(add-hook! :append 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
(add-hook! :append 'emacs-lisp-mode-hook (flycheck-mode 0))

;; clojure
(def-package! clojure-mode
  :mode "\\.cljs?$"
  :config
  (company-mode)
  (flycheck-mode)
  (rainbow-delimiters-mode)
  (setq cider-repl-display-help-banner nil)
  (setq cider-prompt-for-symbol nil))

(def-package! cider
  :after clojure-mode)

(def-package! flycheck-clojure
  :after clojure-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-clojure-setup))

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(map! :map dired-mode-map
         :n "-" #'dired-up-directory)

(add-hook! 'before-save-hook 'whitespace-cleanup)
