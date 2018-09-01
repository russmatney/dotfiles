;;;  -*- lexical-binding: t; -*-

;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;
;; 'Langs' is meant to cover configs for programming languages
;;
;; Ideally there would be no bindings in this file, only mapping
;; from global functions to language specific executions, for example
;; jump-to-definition or jump-to-docs. As Doom and LSP grow more
;; robust, I'd expect this file to take a firmer shape.
;;
;; There's still some back and forth on lsp vs intero for haskell.
;; LSP is promising but not as fully featured or bug-free.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! :append 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
(add-hook! :append 'emacs-lisp-mode-hook (flycheck-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)

(defun grfn/haskell-test-file-p ()
  (string-match-p (rx (and ".hs" eol))
                  (buffer-file-name)))

(defun grfn/intero-run-tests ()
  (interactive)
  (when (grfn/haskell-test-file-p)
    (intero-repl-load)
    (let ((last-buffer (current-buffer)))
      (intero-with-repl-buffer nil
        (comint-simple-send
         (get-buffer-process (current-buffer))
         "main"))
      (switch-to-buffer-other-window last-buffer)
      )))

(after! haskell-mode
  (flycheck-mode)
  (setq evil-shift-width 2)
  (setq haskell-font-lock-symbols t)
  (setq haskell-font-lock-symbols-alist (-reject
                                         (lambda (elem)
                                           (string-equal "()" (car elem)))
                                         haskell-font-lock-symbols-alist)))

(def-package! dante
  :config
  (setq dante-repl-command-line '("stack" "repl" "grid:lib" "grid:grid-test"))
)

;; (def-package! lsp-mode
;;   :after (:any haskell-mode)
;;   :config
;;   (lsp-mode))

;; (def-package! lsp-ui
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-flycheck-enable t)
;;   (setq imenu-auto-rescan t)
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   (lsp-ui-mode . flycheck-mode))

;; (def-package! company-lsp
;;   :after (lsp-mode lsp-ui)
;;   :config
;;   (setq company-backends '(company-lsp))
;;   (setq company-lsp-async t))

;; (def-package! lsp-haskell
;;   :after (lsp-mode lsp-ui haskell-mode)
;;   :hook
;;   (haskell-mode . lsp-haskell-enable))

;; (def-package! intero
;;   :after haskell-mode
;;   :config
;;   (intero-global-mode 1)
;;   (flycheck-add-next-checker 'intero 'haskell-hlint)
;; )

(map!
 (:after haskell-mode
  (:map haskell-mode-map
     :n "g SPC" 'haskell-process-load-file
     :n "g RET" 'grfn/intero-run-tests
     ;; :n "g r"   'lsp-ui-peek-find-references
     ;; :n "g d"   'lsp-ui-peek-find-definitions
     :n "g d"   'xref-find-definitions
     ;; :n "g SPC" 'intero-repl-load
     :n "g a"   'lsp-apply-commands
     :n "g m"   'lsp-ui-imenu
     :n "g i"   'dante-info
     ;; :n "g i"   'haskell-navigate-imports-go
     :n "g b"   'haskell-navigate-imports-return
     :n "g f"   'urbint/format-haskell-source
     (:leader
       (:desc "Format" :prefix "f"
         :desc "format imports" :n "i" 'urbint/format-haskell-imports
         :desc "format file (brittany)" :n "b" 'urbint/format-haskell-source
     )))))

(defun urbint/format-haskell-imports ()
  (interactive)
  (haskell-align-imports)
  (haskell-sort-imports))


(defun urbint/format-haskell-source ()
  (interactive)
  (let ((output-buffer (generate-new-buffer "brittany-out"))
        (config-file-path
         (concat (string-trim
                  (shell-command-to-string "stack path --project-root"))
                 "/brittany.yaml")))
    (when (= 0 (call-process-region
                (point-min) (point-max)
                "stack"
                nil output-buffer nil
                "exec" "--" "brittany" "--config-file" config-file-path))
      (let ((pt (point))
            (wst (window-start))
            (formatted-source (with-current-buffer output-buffer
                                (buffer-string))))
        (erase-buffer)
        (insert formatted-source)
        (goto-char pt)
        (set-window-start nil wst)))))

;; (add-hook
;;  'before-save-hook
;;  (lambda ()
;;    (when (eq major-mode 'haskell-mode)
;;      (urbint/format-haskell-source))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! elixir-mode
  (flycheck-mode)
  (turn-off-smartparens-mode)
  (rainbow-delimiters-mode))

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS + React + Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)
;; ;; eslint integration with flycheck
(setq flycheck-javascript-eslint-executable "~/projects/urbint/grid-front-end/node_modules/.bin/eslint")
;; ;; general css settings
(setq css-indent-offset 2)

(add-hook! js-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

(def-package! flow-minor-mode
  :config
  (add-hook 'js2-mode-hook #'flow-minor-mode))

(map!
 (:after flow-minor-mode
  (:map flow-minor-mode-map
     :n "g d"   'flow-minor-jump-to-definition)))

;; (set-lookup-handlers! 'js2-mode :definition #'flow-minor-jump-to-definition)

(def-package! prettier-js
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook #'prettier-js-mode))

(def-package! add-node-modules-path)

;; (def-package! lsp-javascript-flow
;;   :after (lsp-mode lsp-ui rjsx-mode)
;;   :hook
;;   (rjsx-mode . lsp-javascript-flow-enable))

(def-package! company-flow
  :config
  (defun flow/set-flow-executable ()
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "node_modules/flow-bin"))
           (executable (car (file-expand-wildcards
                               (concat root "node_modules/flow-bin/*osx*/flow")))))
      (setq-local company-flow-executable executable)
      (setq-local flow-minor-default-binary executable)
      (setq-local flycheck-javascript-flow-executable executable)))

  (add-hook 'rjsx-mode-hook #'flow/set-flow-executable)
  (add-to-list 'company-flow-modes 'rjsx-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-flow)))

(def-package! flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(def-package! rjsx-mode
  :bind (:map rjsx-mode-map
              ("<" . nil)
              ("C-d" . nil)
              (">" . nil))
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
)

;; (load! "+emacs-flow-jsx")
;; (def-package! flow-jsx-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GraphQL mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! graphql-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (with-eval-after-load 'lsp-mode
;;   ;; (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
;;   (require 'lsp-rust))

;; (add-hook 'rust-mode-hook #'lsp-rust-enable)
;; (add-hook 'rust-mode-hook #'flycheck-mode)

