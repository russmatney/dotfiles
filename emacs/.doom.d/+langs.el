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
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook! :append 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
;; (add-hook! :append 'emacs-lisp-mode-hook (flycheck-mode 0))


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
      (switch-to-buffer-other-window last-buffer))))


(after! haskell-mode
  (flycheck-mode)
  (setq evil-shift-width 2)
  (setq haskell-font-lock-symbols t)
  (setq haskell-font-lock-symbols-alist (-reject
                                         (lambda (elem)
                                           (string-equal "()" (car elem)))
                                         haskell-font-lock-symbols-alist)))

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
         :desc "format file (brittany)" :n "b" 'urbint/format-haskell-source)))))


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

(use-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(use-package! flycheck-credo
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

(use-package! flow-minor-mode
  :config
  (add-hook 'js2-mode-hook #'flow-minor-mode))

(map!
 (:after flow-minor-mode
   (:map flow-minor-mode-map
     :n "g d"   'flow-minor-jump-to-definition)))

;; (set-lookup-handlers! 'js2-mode :definition #'flow-minor-jump-to-definition)

(use-package! prettier-js
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook #'prettier-js-mode))

(use-package! add-node-modules-path)

(use-package! company-flow
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

(use-package! flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package! rjsx-mode
  :bind (:map rjsx-mode-map
          ("<" . nil)
          ("C-d" . nil)
          (">" . nil))
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))


;; (load! "+emacs-flow-jsx")
;; (use-package! flow-jsx-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GraphQL mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! graphql-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'pipenv-mode 'py-yapf-enable-on-save)

;; (add-hook
;;  'before-save-hook
;;  (lambda ()
;;    (when (eq major-mode 'pipenv-mode)
;;      (py-yapf-buffer))))


(map!
 ;; (:after python-mode
 ;;  (:map python-mode-map
 :n "g d"   '+lookup/definition
 :n "g r"   '+lookup/references
 :n "g f"   'py-yapf-buffer)
;; ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cider-copy-last-result ()
  (interactive)
  (cider-interactive-eval
   "*1"
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (_ value)
      (kill-new value)
      (message "Copied last result (%s) to clipboard"
               (if (= (length value) 1) "1 char"
                 (format "%d chars" (length value)))))
    nil nil nil)))

(map!
 (:after cider-mode
   (:leader
     :desc "Lookup documentation at point" :n  "d"  #'cider-doc
     :desc "Jump to definition at point"   :n  "l"  #'cider-find-var)
   (:map cider-mode-map
     (:leader
       :n "DEL" #'ivy-cider-browse-ns
       :n "\\" #'ivy-cider-apropos
       (:desc "Cider" :prefix "c"
         :n  "'"  #'cider-jack-in
         :n  "\"" #'cider-jack-in-cljs
         :n  "b"  #'cider-eval-buffer
         :n  "B"  #'cider-switch-to-repl-buffer
         :n  "y"  #'cider-copy-last-result
         :n  "n"  #'cider-repl-set-ns
         :n  "j"  #'cider-find-var
         (:desc "docs" :prefix "d"
           :desc "Browse Namespace" :n  "n" #'cider-browse-ns
           :desc "Browse Spec"      :n  "s" #'cider-browse-spec)
         :n  "h"  #'cider-doc
         :n  "c"  #'cider-repl-clear-buffer
         :n  "i"  #'cider-inspect-last-result
         :n  "p"  #'cider-eval-sexp-at-point
         :n  "f"  #'cider-eval-defun-at-point
         :n  "t"  #'cider-test-run-ns-tests
         :n  "T"  #'cider-test-run-test

         :n  "l"  #'clojure-introduce-let
         :n  "m"  #'clojure-move-to-let)))
   (:after cider-browse-ns-mode
     (:map cider-browse-ns-mode-map
       :n "RET"       #'cider-browse-ns-operate-at-point))))

(use-package! flycheck-clj-kondo)

(use-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.edn$"
  :mode "\\(?:build\\|profile\\)\\.boot$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :mode ("\\.cljc$" . clojurec-mode)

  :config
  (require 'flycheck-clj-kondo)

  ;; from https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

  (dolist (checkers '((clj-kondo-clj . clojure-joker)
                      (clj-kondo-cljs . clojurescript-joker)
                      (clj-kondo-cljc . clojure-joker)
                      (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("set" . "clojure.set")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "cheshire.core")
          ("s" . "clojure.spec.alpha")
          ("rf" . "re-frame.core")
          ("r" . "reagent.core"))

        (setq clojure-align-forms-automatically t)

        (setq cider-cljs-lein-repl
              "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")))

(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode)

  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

;; https://github.com/noctuid/lispyville
(use-package! lispyville
  :hook
  (emacs-lisp-mode . lispyville-mode)
  (clojure-mode . lispyville-mode)
  (lisp-mode . lispyville-mode)

  :bind (:map lispyville-mode-map
          ("M-L" . lispyville-beginning-of-next-defun))

  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     text-objects
     atom-motions
     additional-motions
     additional
     additional-insert
     additional-wrap
     commentary
     slurp/barf-lispy
     wrap))
  (setq
   lispy-safe-actions-ignore-strings t
   lispy-safe-actions-ignore-comments t))

(use-package! ivy-cider
  :after cider-mode)

;;(remove-hook 'clojure-mode-hook 'parinfer-mode)
;; (remove-hook 'emacs-lisp-mode-hook 'parinfer-mode)


(defmacro define-move-and-insert
    (name &rest body)
  `(defun ,name (count &optional vcount skip-empty-lines)
     ;; Following interactive form taken from the source for `evil-insert'
     (interactive
      (list (prefix-numeric-value current-prefix-arg)
            (and (evil-visual-state-p)
                 (memq (evil-visual-type) '(line block))
                 (save-excursion
                   (let ((m (mark)))
                     ;; go to upper-left corner temporarily so
                     ;; `count-lines' yields accurate results
                     (evil-visual-rotate 'upper-left)
                     (prog1 (count-lines evil-visual-beginning evil-visual-end)
                       (set-mark m)))))
            (evil-visual-state-p)))
     (atomic-change-group
       ,@body
       (evil-insert count vcount skip-empty-lines))))

(define-move-and-insert grfn/insert-at-sexp-end
  (when (not (equal (get-char) "("))
    (backward-up-list))
  (forward-sexp)
  (backward-char))

(define-move-and-insert grfn/insert-at-sexp-start
  (backward-up-list)
  (forward-char))

(define-move-and-insert grfn/insert-at-form-start
  (backward-sexp)
  (backward-char)
  (insert " "))

(define-move-and-insert grfn/insert-at-form-end
  (forward-sexp)
  (insert " "))
