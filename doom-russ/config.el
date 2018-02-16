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
  (load! +haskell)
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

(setq
 whitespace-line-column 100
 whitespace-style
 '(face trailing lines-tail)
 )
(global-whitespace-mode t)

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
  ;; :mode "\\.exs?$"
  ;; :config
  (flycheck-mode)
  (turn-off-smartparens-mode)
  (rainbow-delimiters-mode)
  ;; (setq alchemist-goto-elixir-source-dir "/path/to/elixir/source/")
  ;; (setq alchemist-goto-erlang-source-dir "/path/to/erlang/source/")

  )

;; (defun custom-erlang-mode-hook ()
;;   (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

;; (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)



(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
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

(def-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; (map! :map dired-mode-map
;;          :n "-" #'dired-up-directory
;;          :n "<return>" #'dired-find-file
;;          )

(add-hook! 'before-save-hook 'whitespace-cleanup)
