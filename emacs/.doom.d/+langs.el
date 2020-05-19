;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; LSP

(use-package! lsp-mode
  :hook
  (haskell-mode . lsp)
  ;;(python-mode . lsp)
  (rust-mode . lsp)
  :commands
  lsp)

(after! rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (setq rust-format-on-save t))

(use-package! lsp-ui
  :commands
  lsp-ui-mode)

(use-package! company-lsp
  :commands company-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after haskell-mode
   (:map haskell-mode-map
     :n "g SPC" 'haskell-process-load-file
     ;; :n "g r"   'lsp-ui-peek-find-references
     ;; :n "g d"   'lsp-ui-peek-find-definitions
     :n "g d"   'xref-find-definitions
     ;; :n "g SPC" 'intero-repl-load
     :n "g a"   'lsp-apply-commands
     :n "g m"   'lsp-ui-imenu
     :n "g i"   'dante-info
     ;; :n "g i"   'haskell-navigate-imports-go
     :n "g b"   'haskell-navigate-imports-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! elixir-mode
  (flycheck-mode)
  (turn-off-smartparens-mode)
  (rainbow-delimiters-mode))

(use-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(use-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)
(setq css-indent-offset 2)

(use-package! prettier-js
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook #'prettier-js-mode))

(use-package! add-node-modules-path)

(use-package! rjsx-mode
  :bind (:map rjsx-mode-map
          ("<" . nil)
          ("C-d" . nil)
          (">" . nil))
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Csharp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! csharp-mode
  :config
  (electric-pair-local-mode 1)

  (add-hook! 'omnisharp-mode-hook
    (add-hook 'before-save-hook #'omnisharp-code-format-entire-file))

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (add-hook 'omnisharp-mode-hook
            '(lambda () (c-set-style "ellemtel")))
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))
