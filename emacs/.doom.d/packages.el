;;; packages.el -*- lexical-binding: t; -*-

;; lisp
(package! dash)

;; javascript
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)

;; lsp
(package! lsp-mode)
(package! lsp-ui :recipe (:host github :repo "emacs-lsp/lsp-ui"))
(package! company-lsp)

;; rust
;;(package! lsp-rust)

;; fountain
(package! fountain-mode :recipe (:host github :repo "rnkn/fountain-mode"))

;; python
(package! py-yapf)

;; clojure
(package! aggressive-indent)
(package! lispy)
(package! lispyville)

(package! flycheck-clj-kondo)
(package! ivy-cider :recipe (:host github :repo "rschmukler/ivy-cider"))

;; wakatime
(package! wakatime-mode)

;; exwm
(package! exwm)
