;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; LSP

;; https://www.reddit.com/r/emacs/comments/hw0cqq/does_anyone_have_issues_with_lspuidoc/
(defvar lva--lsp-ui-doc-atpoint-h 10
  "lsp-ui-doc-max-height when lsp-ui is shown at point")
(defvar lva--lsp-ui-doc-atpoint-w 50
  "lsp-ui-doc-max-width when lsp-ui is shown at point")
(defvar lva--lsp-ui-doc-anchored-h 20
  "lsp-ui-doc-max-height when lsp-ui position is 'top or 'bottom")
(defvar lva--lsp-ui-doc-anchored-w 150
  "lsp-ui-doc-max-width when lsp-ui position is 'top or 'bottom")

(defun lva/lsp-ui-toggle-doc (arg)
  (interactive "P")
  (if lsp-ui-doc-mode
      (lsp-ui-doc-mode 0)
    (progn
      (if arg
          (setq lsp-ui-doc-position 'at-point
                lsp-ui-doc-max-height lva--lsp-ui-doc-atpoint-h
                lsp-ui-doc-max-width lva--lsp-ui-doc-atpoint-w)
        (setq lsp-ui-doc-position 'top
              lsp-ui-doc-max-height lva--lsp-ui-doc-anchored-h
              lsp-ui-doc-max-width lva--lsp-ui-doc-anchored-w))
      (lsp-ui-doc-mode))))

(use-package! lsp-mode
  :hook
  (haskell-mode . lsp)
  ;; (python-mode . lsp)
  (rust-mode . lsp)
  ;; :commands lsp
  :config
  ;; (lsp-ui-doc-mode 0)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-keymap-prefix "SPC l")
  ;; (lsp-ui-doc-mode nil)
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-keymap-prefix "SPC l")


  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

  ;; rust
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-import-merge-behaviour "last"
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-watch-args ["--target-dir" "/tmp/rust-analyzer-check"]
        lsp-ui-doc-enable t)
  )


(map! :after lsp-mode
      :map lsp-mode-map
      "s-l" nil
      "SPC" nil
      )

;; (defun russ/setup-rust ()
;;   (setq rust-format-on-save t)
;;   (setq rust-format-show-buffer nil)
;;   (rust-enable-format-on-save)
;;   (lsp))

;; ;; (use-package! rust-mode
;; ;;   :config
;; ;;   (russ/setup-rust))

;; (add-hook 'rust-mode-hook #'russ/setup-rust)

(after! rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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
(setq typescript-indent-level 2)

(use-package! prettier-js
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-tsx-mode #'prettier-js-mode))

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

;; (use-package! csharp-mode
;;   :config
;;   ;; (electric-pair-local-mode 1)

;;   (add-hook! 'omnisharp-mode-hook
;;     (add-hook 'before-save-hook #'omnisharp-code-format-entire-file))

;;   (add-hook! 'omnisharp-mode-hook
;;     (whitespace-mode -1))

;;   (setq indent-tabs-mode nil)
;;   (setq c-syntactic-indentation nil)
;;   ;; (add-hook 'omnisharp-mode-hook
;;   ;;           '(lambda () (c-set-style "ellemtel")))
;;   (setq c-basic-offset 4)
;;   (setq truncate-lines t)
;;   (setq tab-width 4)
;;   (setq evil-shift-width 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fennel and Lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flycheck-define-checker fennel-lua-luacheck
  "Some docstring"
  :command ("fennelcheck"
            "--formatter" "plain"
            "--codes"                   ; Show warning codes
            "--no-color"
            (option-list "--std" flycheck-luacheck-standards)
            (config-file "--config" flycheck-luacheckrc)
            ;; TODO might be saner to use .lua here, b/c output doesnt really match .fnl
            "--filename" source-original
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((warning line-start
            (optional (file-name))
            ":" line ":" column
            ": (" (id "W" (one-or-more digit)) ") "
            (message) line-end)
   (error line-start
          (optional (file-name))
          ":" line ":" column ":"
          ;; `luacheck' before 0.11.0 did not output codes for errors, hence
          ;; the ID is optional here
          (optional " (" (id "E" (one-or-more digit)) ") ")
          (message) line-end))
  :modes fennel-mode)

;; (flycheck-define-checker lua
;; " doc string"
;;   :command ("luac" "-p" "-")
;;   :standard-input t
;;   :error-patterns
;;   ((error line-start
;;           ;; Skip the name of the luac executable.
;;           (minimal-match (zero-or-more not-newline))
;;           ": stdin:" line ": " (message) line-end))
;;   :modes lua-mode)

(use-package! fennel-mode
  :hook (fennel-mode . rainbow-delimiters-mode)
  :config
  (add-to-list 'flycheck-checkers 'fennel-lua-luacheck)
  ;; (add-hook 'fennel-mode-hook #'fennel-enable-monroe)
  (setq fennel-mode-switch-to-repl-after-reload nil))

(after! fennel-mode
  (map! :map fennel-mode-map
    :localleader
    "z" #'fennel-repl
    "k" #'russ/love-module-reload
    "l" #'fennel-view-compilation
    ;; "r" #'russ/open-love-repl
    "R" #'russ/love-kill-and-restart-via-tmux))

(use-package! lua-mode
  :hook (lua-mode . rainbow-delimiters-mode))

(after! lua-mode
  (map! :map lua-mode-map
    :localleader
    "k" #'russ/love-module-reload
    "r" #'russ/open-love-repl
    "R" #'russ/love-kill-and-restart-via-tmux))

(use-package! friar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fix for python-pytest in monorepos
;; https://github.com/wbolster/emacs-python-pytest
;; (add-hook 'python-mode-hook
;;              (lambda ()
;;                (when-let ((r (locate-dominating-file default-directory ".pyroot")))
;;                  (setq python-pytest-executable
;;                        (concat "PYTHONPATH=" r " nix-shell --run -- pytest")))))

(use-package! python-pytest
  :commands python-pytest-dispatch
  :init
  (map! :after python
        :localleader
        :map python-mode-map
        :prefix ("t" . "test")
        "a" #'python-pytest
        "f" #'python-pytest-file-dwim
        "F" #'python-pytest-file
        "t" #'python-pytest-function-dwim
        "T" #'python-pytest-function
        "r" #'python-pytest-repeat
        "p" #'python-pytest-dispatch
        "l" #'python-pytest-last-failed))
